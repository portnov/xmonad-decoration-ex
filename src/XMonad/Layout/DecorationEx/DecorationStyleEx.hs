{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module XMonad.Layout.DecorationEx.DecorationStyleEx where

import Control.Monad
import Data.Bits (testBit)
import Foreign.C.Types (CInt)

import XMonad
import XMonad.Prelude
import qualified XMonad.StackSet as W
import XMonad.Layout.Decoration (Shrinker (..), isInStack, shrinkWhile)
import XMonad.Layout.DraggingVisualizer (DraggingVisualizerMsg (..))
import XMonad.Layout.DecorationAddons (handleScreenCrossing)
import XMonad.Util.Font
import XMonad.Util.NamedWindows (getName)
import XMonad.Hooks.UrgencyHook

import XMonad.Layout.DecorationEx.Types

instance (Show widget, Read widget) => ThemeAttributes (ThemeEx widget) where
  type Style (ThemeEx widget) = SimpleStyle
  selectWindowStyle theme w = windowStyle w theme
  defaultBgColor t = sBgColor $ exInactive t
  decorationSize t = (exDecoWidth t, exDecoHeight t)
  widgetsPadding = exPadding
  themeFontName = exFontName

data DrawData dstyle = DrawData {
    ddFont :: XMonadFont
  , ddStyle :: Style (Theme dstyle (Widget dstyle))
  , ddOrigWindow :: Window
  , ddWindowTitle :: String
  , ddDecoRect :: Rectangle
  , ddLabels :: WidgetLayout (Widget dstyle)
  , ddWidgetPlaces :: [WidgetPlace]
  }

type ThemeW dstyle = Theme dstyle (Widget dstyle)

class (Read (dstyle a), Show (dstyle a),
       Eq a,
       DecorationWidget (Widget dstyle),
       HasWidgets (Theme dstyle) (Widget dstyle),
       ThemeAttributes (Theme dstyle (Widget dstyle)))
    => DecorationStyleEx dstyle a where

    type Theme dstyle :: * -> *
    type Widget dstyle

    describeDecoration :: dstyle a -> String

    shrinkWindow :: dstyle a -> Rectangle -> Rectangle -> Rectangle
    shrinkWindow _ (Rectangle _ _ _ dh) (Rectangle x y w h) = Rectangle x (y + fi dh) w (h - dh)

    -- | The pure version of the main method, 'decorate'.
    pureDecoration :: dstyle a -> ThemeW dstyle -> Rectangle
                   -> W.Stack a -> [(a,Rectangle)] -> (a,Rectangle) -> Maybe Rectangle
    pureDecoration _ theme _ s _ (w, Rectangle x y windowWidth windowHeight) =
      let (decoWidth, decoHeight) = decorationSize theme
      in  if isInStack s w && (decoHeight < windowHeight)
            then Just $ Rectangle x y windowWidth decoHeight
            else Nothing

    decorateWindow :: dstyle a -> ThemeW dstyle -> Rectangle
        -> W.Stack a -> [(a, Rectangle)] -> (a, Rectangle) -> X (Maybe Rectangle)
    decorateWindow dstyle theme r s wrs wr = return $ pureDecoration dstyle theme r s wrs wr

    calcWidgetPlace :: dstyle a -> DrawData dstyle -> Widget dstyle -> X WidgetPlace

    placeWidgets :: Shrinker shrinker => dstyle a -> ThemeW dstyle -> shrinker -> XMonadFont -> Rectangle -> Window -> WidgetLayout (Widget dstyle) -> X [WidgetPlace]

    decorationEventHookEx :: Shrinker shrinker => dstyle a -> ThemeW dstyle -> DecorationStateEx -> shrinker -> Event -> X ()
    decorationEventHookEx = handleMouseFocusDrag

    handleDecorationClick :: dstyle a -> ThemeW dstyle -> Rectangle -> [Rectangle] -> Window -> Int -> Int -> X Bool
    handleDecorationClick = decorationHandler

    decorationWhileDraggingHook :: dstyle a -> CInt -> CInt -> (Window, Rectangle) -> Position -> Position -> X ()
    decorationWhileDraggingHook _ = handleDraggingInProgress

    -- | This hoook is called after a window has been dragged using the decoration.
    decorationAfterDraggingHook :: dstyle a -> (Window, Rectangle) -> Window -> X ()
    decorationAfterDraggingHook _ds (w, _r) decoWin = do
      focus w
      hasCrossed <- handleScreenCrossing w decoWin
      unless hasCrossed $ do
        sendMessage DraggingStopped
        performWindowSwitching w

    paintDecoration :: dstyle a -> a -> Dimension -> Dimension -> DrawData dstyle -> X()
    -- FIXME: Передавать не DrawData (со списком остальных виджетов зачем-то),
    -- а более скромную структуру
    paintWidget :: dstyle a -> DecorationPaintingContext -> WidgetPlace -> DrawData dstyle -> Widget dstyle -> X ()

handleDraggingInProgress :: CInt -> CInt -> (Window, Rectangle) -> Position -> Position -> X ()
handleDraggingInProgress ex ey (mainw, r) x y = do
    let rect = Rectangle (x - (fi ex - rect_x r))
                         (y - (fi ey - rect_y r))
                         (rect_width  r)
                         (rect_height r)
    sendMessage $ DraggingWindow mainw rect

performWindowSwitching :: Window -> X ()
performWindowSwitching win =
    withDisplay $ \d -> do
       root <- asks theRoot
       (_, _, selWin, _, _, _, _, _) <- io $ queryPointer d root
       ws <- gets windowset
       let allWindows = W.index ws
       -- do a little double check to be sure
       when ((win `elem` allWindows) && (selWin `elem` allWindows)) $ do
                let allWindowsSwitched = map (switchEntries win selWin) allWindows
                let (ls, notEmpty -> t :| rs) = break (win ==) allWindowsSwitched
                let newStack = W.Stack t (reverse ls) rs
                windows $ W.modify' $ const newStack
    where
        switchEntries a b x
            | x == a    = b
            | x == b    = a
            | otherwise = x

alignLeft :: forall dstyle a. DecorationStyleEx dstyle a => dstyle a -> DrawData dstyle -> [Widget dstyle] -> X [WidgetPlace]
alignLeft dstyle dd widgets = do
    places <- mapM (calcWidgetPlace dstyle dd) widgets
    let places' = go (rect_x $ ddDecoRect dd) places
    return places'
  where
    go _ [] = []
    go x0 (place : places) =
      let rect = wpRectangle place
          x' = x0 + rect_x rect
          rect' = rect {rect_x = x'}
          place' = place {wpRectangle = rect'}
      in  place' : go (x' + fi (rect_width rect)) places

alignRight :: forall dstyle a. DecorationStyleEx dstyle a => dstyle a -> DrawData dstyle -> [Widget dstyle] -> X [WidgetPlace]
alignRight dstyle dd widgets = do
    places <- mapM (calcWidgetPlace dstyle dd) widgets
    return $ reverse $ go (rect_width $ ddDecoRect dd) places
  where
    go _ [] = []
    go x0 (place : places) = 
      let rect = wpRectangle place
          x' = x0 - rect_width rect
          rect' = rect {rect_x = fi x'}
          place' = place {wpRectangle = rect'}
      in  place' : go x' places

alignCenter :: forall dstyle a. DecorationStyleEx dstyle a => dstyle a -> DrawData dstyle -> [Widget dstyle] -> X [WidgetPlace]
alignCenter dstyle dd widgets = do
    places <- alignLeft dstyle dd widgets
    let totalWidth = sum $ map (rect_width . wpRectangle) places
        x0 = (rect_width (ddDecoRect dd) - fi totalWidth) `div` 2
    return $ map (shift x0) places
  where
    shift x0 place =
      let rect = wpRectangle place
          rect' = rect {rect_x = rect_x rect + fi x0}
      in  place {wpRectangle = rect'}

mkDrawData :: (Shrinker shrinker, ThemeAttributes (ThemeW dstyle), HasWidgets (Theme dstyle) (Widget dstyle))
           => shrinker -> ThemeW dstyle -> XMonadFont -> Window -> Rectangle -> X (DrawData dstyle)
mkDrawData shrinker theme font origWindow decoRect@(Rectangle _ _ wh ht) = do
    name <- getShrinkedWindowName shrinker font origWindow wh ht
    style <- selectWindowStyle theme origWindow
    return $ DrawData {
                   ddFont = font,
                   ddStyle = style,
                   ddOrigWindow = origWindow,
                   ddWindowTitle = name,
                   ddDecoRect = decoRect,
                   ddLabels = themeWidgets theme,
                   ddWidgetPlaces = []
                  }

windowStyle :: Window -> ThemeEx widget -> X SimpleStyle
windowStyle win theme = do
  mbFocused <- W.peek <$> gets windowset
  isWmStateUrgent <- (win `elem`) <$> readUrgents
  isUrgencyBitSet <- withDisplay $ \dpy -> do
                       hints <- io $ getWMHints dpy win
                       return $ wmh_flags hints `testBit` urgencyHintBit
  if isWmStateUrgent || isUrgencyBitSet
    then return $ exUrgent theme
    else return $
      case mbFocused of
        Nothing -> exInactive theme
        Just focused
          | focused == win -> exActive theme
          | otherwise -> exInactive theme

getShrinkedWindowName :: Shrinker s => s -> XMonadFont -> Window -> Dimension -> Dimension -> X String
getShrinkedWindowName shrinker font win wh ht = do
  -- xmonad-contrib #809
  -- qutebrowser will happily shovel a 389K multiline string into @_NET_WM_NAME@
  -- and the 'defaultShrinker' (a) doesn't handle multiline strings well (b) is
  -- quadratic due to using 'init'
  nw  <- fmap (take 2048 . takeWhile (/= '\n') . show) (getName win)
  let s = shrinkIt shrinker
  dpy <- asks display
  shrinkWhile s (\n -> do size <- io $ textWidthXMF dpy font n
                          return $ size > fromIntegral wh - fromIntegral (ht `div` 2)) nw

-- | Mouse focus and mouse drag are handled by the same function, this
-- way we can start dragging unfocused windows too.
handleMouseFocusDrag :: (DecorationStyleEx dstyle a, Shrinker shrinker) => dstyle a -> ThemeW dstyle -> DecorationStateEx -> shrinker -> Event -> X ()
handleMouseFocusDrag ds theme (DecorationStateEx {dsDecorations, dsFont}) shrinker (ButtonEvent {ev_window, ev_x_root, ev_y_root, ev_event_type})
    | ev_event_type == buttonPress
    , Just (WindowDecoration {..}) <- findDecoDataByDecoWindow ev_window dsDecorations = do
        let decoRect@(Rectangle dx dy _ _) = fromJust wdDecoRect
            x = fi $ ev_x_root - fi dx
            y = fi $ ev_y_root - fi dy
        dealtWith <- handleDecorationClick ds theme decoRect (map wpRectangle wdWidgets) wdOrigWindow x y
        unless dealtWith $
            mouseDrag (\x y -> focus wdOrigWindow >> decorationWhileDraggingHook ds ev_x_root ev_y_root (wdOrigWindow, wdOrigWinRect) x y)
                      (decorationAfterDraggingHook ds (wdOrigWindow, wdOrigWinRect) ev_window)
handleMouseFocusDrag _ _ _ _ _ = return ()

-- | Given a window and the state, if a matching decoration is in the
-- state return it with its ('Maybe') 'Rectangle'.
findDecoDataByDecoWindow :: Window -> [WindowDecoration] -> Maybe WindowDecoration
findDecoDataByDecoWindow decoWin = find (\dd -> wdDecoWindow dd == Just decoWin)

decorationHandler :: forall dstyle a.
                     (DecorationStyleEx dstyle a)
                  => dstyle a
                  -> ThemeW dstyle
                  -> Rectangle
                  -> [Rectangle]
                  -> Window
                  -> Int
                  -> Int
                  -> X Bool
decorationHandler deco theme decoRect widgetPlaces window x y = do
    go $ zip (widgetLayout $ themeWidgets theme) widgetPlaces
  where
    go :: [(Widget dstyle, Rectangle)] -> X Bool
    go [] = return False
    go ((w, rect) : rest) = do
      if pointWithin (fi x) (fi y) rect
        then do
          executeWindowCommand (widgetCommand w) window
          return True
        else go rest

defaultPaintDecoration :: forall dstyle.
                          (DecorationStyleEx dstyle Window, Style (Theme dstyle (Widget dstyle)) ~ SimpleStyle)
                       => dstyle Window
                       -> Window
                       -> Dimension
                       -> Dimension
                       -> DrawData dstyle
                       -> X ()
defaultPaintDecoration deco win windowWidth windowHeight dd = do
    dpy <- asks display
    let widgets = widgetLayout $ ddLabels dd
        style = ddStyle dd
    pixmap  <- io $ createPixmap dpy win windowWidth windowHeight (defaultDepthOfScreen $ defaultScreenOfDisplay dpy)
    gc <- io $ createGC dpy pixmap
    -- draw
    io $ setGraphicsExposures dpy gc False
    bgColor <- stringToPixel dpy (sBgColor style)
    -- we start with the border
    let borderWidth = sDecoBorderWidth style
        borderColors = sDecorationBorders style
    drawLineWith dpy pixmap gc 0 0 windowWidth borderWidth (bxTop borderColors)
    drawLineWith dpy pixmap gc 0 0 borderWidth windowHeight (bxLeft borderColors)
    drawLineWith dpy pixmap gc 0 (fi (windowHeight - borderWidth)) windowWidth borderWidth (bxBottom borderColors)
    drawLineWith dpy pixmap gc (fi (windowWidth - borderWidth)) 0 borderWidth windowHeight (bxRight borderColors)

    -- and now again
    io $ setForeground dpy gc bgColor
    io $ fillRectangle dpy pixmap gc (fi borderWidth) (fi borderWidth) (windowWidth - (borderWidth * 2)) (windowHeight - (borderWidth * 2))

    -- paint strings
    forM_ (zip widgets $ ddWidgetPlaces dd) $ \(widget, place) ->
        paintWidget deco (dpy, pixmap, gc) place dd widget

    -- debug
    -- black <- stringToPixel dpy "black"
    -- io $ setForeground dpy gc black
    -- forM_ (ddWidgetPlaces dd) $ \(WidgetPlace {wpRectangle = Rectangle x y w h}) ->
    --   io $ drawRectangle dpy pixmap gc x y w h

    -- copy the pixmap over the window
    io $ copyArea      dpy pixmap win gc 0 0 windowWidth windowHeight 0 0
    -- free the pixmap and GC
    io $ freePixmap    dpy pixmap
    io $ freeGC        dpy gc
  where
    drawLineWith dpy pixmap gc x y w h colorName = do
      color <- stringToPixel dpy colorName
      io $ setForeground dpy gc color
      io $ fillRectangle dpy pixmap gc x y w h

defaultPlaceWidgets :: (Shrinker shrinker, DecorationStyleEx dstyle a, ThemeAttributes (ThemeW dstyle))
                    => dstyle a -> ThemeW dstyle -> shrinker -> XMonadFont -> Rectangle -> Window -> WidgetLayout (Widget dstyle) -> X [WidgetPlace]
defaultPlaceWidgets deco theme shrinker font decoRect window wlayout = do
    let leftWidgets = wlLeft wlayout
        rightWidgets = wlRight wlayout
        centerWidgets = wlCenter wlayout

    dd <- mkDrawData shrinker theme font window decoRect
    let dd' = dd {ddDecoRect = pad (widgetsPadding theme) (ddDecoRect dd)}
    rightRects <- alignRight deco dd' rightWidgets
    leftRects <- alignLeft deco dd' leftWidgets
    let leftWidgetsWidth = sum $ map (rect_width . wpRectangle) leftRects
        rightWidgetsWidth = sum $ map (rect_width . wpRectangle) rightRects
        dd'' = dd' {ddDecoRect = padCenter leftWidgetsWidth rightWidgetsWidth (ddDecoRect dd)}
    centerRects <- alignCenter deco dd'' centerWidgets
    return $ leftRects ++ centerRects ++ rightRects
  where
    pad p (Rectangle x y w h) =
      Rectangle (fi (bxLeft p)) (fi (bxTop p))
                (w - bxLeft p - bxRight p)
                (h - bxTop p - bxBottom p)
  
    padCenter left right = pad (BoxBorders 0 right 0 left)

