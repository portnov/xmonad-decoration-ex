{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DefaultSignatures #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.DecorationEx.Engines
-- Description :  Type class and it's default implementation for window decoration engines.
-- Copyright   :  (c) 2007 Andrea Rossato, 2009 Jan Vornberger, 2023 Ilya Portnov
-- License     :  BSD-style (see xmonad/LICENSE)
--
-- Maintainer  :  portnov84@rambler.ru
-- Stability   :  unstable
-- Portability :  unportable
--
-- This module defines @DecorationEngine@ type class, and default implementation for it.
-----------------------------------------------------------------------------

module XMonad.Layout.DecorationEx.Engines where

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

instance HasDecorationSize (ThemeEx widget) where
  decorationSize t = (exDecoWidth t, exDecoHeight t)

instance (Show widget, Read widget, Read (WidgetCommand widget), Show (WidgetCommand widget))
        => ThemeAttributes (ThemeEx widget) where
  type Style (ThemeEx widget) = SimpleStyle
  selectWindowStyle theme w = genericWindowStyle w theme
  defaultBgColor t = sBgColor $ exInactive t
  widgetsPadding = exPadding
  themeFontName = exFontName

-- | Auxiliary type for data which are passed from
-- decoration layout modifier to decoration engine.
data DrawData engine = DrawData {
    ddStyleState :: !(DecorationEngineState engine)     -- ^ Decoration engine state
  , ddStyle :: !(Style (Theme engine (Widget engine)))  -- ^ Graphics style of the decoration. This defines colors, fonts etc
                                                        -- which are to be used for this particular window in it's current state.
  , ddOrigWindow :: !Window                             -- ^ Original window to be decorated
  , ddWindowTitle :: !String                            -- ^ Original window title (not shrinked yet)
  , ddDecoRect :: !Rectangle                            -- ^ Decoration rectangle
  , ddLabels :: !(WidgetLayout (Widget engine))         -- ^ Widgets to be placed on decoration
  , ddWidgetPlaces :: !(WidgetLayout WidgetPlace)       -- ^ Places where widgets must be shown
  }

-- | Convinience type synonym
type ThemeW engine = Theme engine (Widget engine)

-- | State of decoration engine
data DecorationLayoutState engine = DecorationLayoutState {
    dsStyleState :: !(DecorationEngineState engine) -- ^ Engine-specific state
  , dsDecorations :: ![WindowDecoration]            -- ^ Mapping between decoration windows and original windows
  }

-- | Decoration geometry class.
-- Decoration geometyr is responsible for placement of window decorations: whether
-- they should be on the top of the window or on the bottom, should they go for 
-- full window widht or only be of certain width, etc.
-- This does not know what will be drawn inside decorations.
class (Read (geom a), Show (geom a),
       Eq a)
    => DecorationGeometry geom a where

    -- | Give a name to decoration geometry implementation.
    describeGeometry :: geom a -> String

    -- | Reduce original window size to make space for decoration, if necessary.
    shrinkWindow :: geom a -> Rectangle -> Rectangle -> Rectangle
    shrinkWindow _ (Rectangle _ _ _ dh) (Rectangle x y w h) = Rectangle x (y + fi dh) w (h - dh)

    -- | The pure version of the main method, 'decorate'.
    -- The method should return a rectangle where to place window decoration,
    -- or Nothing if this window is not to be decorated.
    pureDecoration :: geom a          -- ^ Decoration geometry instance
                   -> DecorationSize  -- ^ Size of decoration rectangle, as defined by theme
                   -> Rectangle       -- ^ Screen rectangle
                   -> W.Stack a       -- ^ Current stack of windows being displayed
                   -> [(a,Rectangle)] -- ^ Set of all windows with their corresponding rectangle
                   -> (a,Rectangle)   -- ^ Window being decorated and it's rectangle
                   -> Maybe Rectangle
    pureDecoration _ (decoWidth, decoHeight) _ s _ (w, Rectangle x y windowWidth windowHeight) =
      if isInStack s w && (decoHeight < windowHeight)
        then Just $ Rectangle x y windowWidth decoHeight
        else Nothing

    -- | The method should return a rectangle where to place window decoration,
    -- or Nothing if this window is not to be decorated.
    decorateWindow :: geom a           -- ^ Decoration geometry instance
                   -> DecorationSize   -- ^ Size of decoration rectangle, as defined by theme
                   -> Rectangle        -- ^ Screen rectangle
                   -> W.Stack a        -- ^ Current stack of windows being displayed
                   -> [(a, Rectangle)] -- ^ Set of all windows with their corresponding rectangle
                   -> (a, Rectangle)   -- ^ Window being decorated and it's rectangle
                   -> X (Maybe Rectangle)
    decorateWindow geom size r s wrs wr = return $ pureDecoration geom size r s wrs wr

-- | Data type for default implementation of DecorationGeometry.
-- This defines simple decorations: a horizontal bar at the top of each window,
-- running for full width of the window.
data DefaultGeometry a = DefaultGeometry
  deriving (Read, Show)

instance Eq a => DecorationGeometry DefaultGeometry a where
  describeGeometry _ = "FullWidth"

-- | Decoration engines type class.
-- Decoration engine is responsible for drawing something inside decoration rectangle.
-- It is also responsible for handling X11 events (such as clicks) which happen
-- within decoration rectangle.
-- Decoration rectangles are defined by DecorationGeometry implementation.
class (Read (engine a), Show (engine a),
       Eq a,
       DecorationWidget (Widget engine),
       HasWidgets (Theme engine) (Widget engine),
       ClickHandler (Theme engine) (Widget engine),
       ThemeAttributes (Theme engine (Widget engine)))
    => DecorationEngine engine a where

    type Theme engine :: * -> *           -- ^ Type of themes used by decoration engine.
                                          -- This type must be parametrized over widget type,
                                          -- because theme will contain a list of widgets.
    type Widget engine                    -- ^ Decoration widget type used by decoration engine.
    type DecorationPaintingContext engine -- ^ Type of data used by engine as a context during painting;
                                          -- for plain X11-based implementation this is Display, Pixmap
                                          -- and GC.
    type DecorationEngineState engine     -- ^ Type of state used by the decoration engine.
                                          -- ^ This can contain some resources that should be initialized
                                          -- and released at time, such as X11 fonts.

    -- | Give a name to decoration engine.
    describeEngine :: engine a -> String

    -- | Initialize state of the engine.
    initializeState :: engine a        -- ^ Decoration engine instance
                    -> geom a          -- ^ Decoration geometry instance
                    -> ThemeW engine   -- ^ Theme to be used
                    -> X (DecorationEngineState engine)

    -- | Release resources held in engine state.
    releaseStateResources :: engine a                     -- ^ Decoration engine instance
                          -> DecorationEngineState engine -- ^ Engine state
                          -> X ()

    -- | Calculate place which will be occupied by one widget.
    -- This method is expected to always return a rectangle which has X = 0,
    -- i.e. at the very left of available space. This is because this method
    -- is not responsible for calculating resulting placement of the widget
    -- on the decoration bar, it only should calculate how much space the widget
    -- will take, so that later we could place the widget corresponding to
    -- widgets order and alignments.
    calcWidgetPlace :: engine a -> DrawData engine -> Widget engine -> X WidgetPlace

    -- | Place widgets along the decoration bar.
    placeWidgets :: Shrinker shrinker
                 => engine a                     -- ^ Decoration engine instance
                 -> ThemeW engine                -- ^ Theme to be used
                 -> shrinker                     -- ^ Strings shrinker
                 -> DecorationEngineState engine -- ^ Current state of the engine
                 -> Rectangle                    -- ^ Decoration rectangle
                 -> Window                       -- ^ Original window to be decorated
                 -> WidgetLayout (Widget engine) -- ^ Widgets layout
                 -> X (WidgetLayout WidgetPlace)

    getShrinkedWindowName :: Shrinker shrinker => engine a -> shrinker -> DecorationEngineState engine -> String -> Dimension -> Dimension -> X String

    default getShrinkedWindowName :: (Shrinker shrinker, DecorationEngineState engine ~ XMonadFont)
                                  => engine a -> shrinker -> DecorationEngineState engine -> String -> Dimension -> Dimension -> X String
    getShrinkedWindowName engine shrinker font name wh ht = do
      let s = shrinkIt shrinker
      dpy <- asks display
      shrinkWhile s (\n -> do size <- io $ textWidthXMF dpy font n
                              return $ size > fromIntegral wh - fromIntegral (ht `div` 2)) name

    decorationXEventMask :: engine a -> EventMask
    decorationXEventMask _ = exposureMask .|. buttonPressMask

    decorationEventHookEx :: Shrinker shrinker => engine a -> ThemeW engine -> DecorationLayoutState engine -> shrinker -> Event -> X ()
    decorationEventHookEx = handleMouseFocusDrag

    handleDecorationClick :: engine a -> ThemeW engine -> Rectangle -> [Rectangle] -> Window -> Int -> Int -> Int -> X Bool
    handleDecorationClick = decorationHandler

    decorationWhileDraggingHook :: engine a -> CInt -> CInt -> (Window, Rectangle) -> Position -> Position -> X ()
    decorationWhileDraggingHook _ = handleDraggingInProgress

    -- | This hoook is called after a window has been dragged using the decoration.
    decorationAfterDraggingHook :: engine a -> (Window, Rectangle) -> Window -> X ()
    decorationAfterDraggingHook _ds (w, _r) decoWin = do
      focus w
      hasCrossed <- handleScreenCrossing w decoWin
      unless hasCrossed $ do
        sendMessage DraggingStopped
        performWindowSwitching w

    paintDecoration :: Shrinker shrinker => engine a -> a -> Dimension -> Dimension -> shrinker -> DrawData engine -> X()
    -- FIXME: Передавать не DrawData (со списком остальных виджетов зачем-то),
    -- а более скромную структуру
    paintWidget :: Shrinker shrinker => engine a -> DecorationPaintingContext engine -> WidgetPlace -> shrinker -> DrawData engine -> Widget engine -> X ()

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

alignLeft :: forall engine a. DecorationEngine engine a => engine a -> DrawData engine -> [Widget engine] -> X [WidgetPlace]
alignLeft engine dd widgets = do
    places <- mapM (calcWidgetPlace engine dd) widgets
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

alignRight :: forall engine a. DecorationEngine engine a => engine a -> DrawData engine -> [Widget engine] -> X [WidgetPlace]
alignRight engine dd widgets = do
    places <- mapM (calcWidgetPlace engine dd) widgets
    return $ reverse $ go (rect_width $ ddDecoRect dd) places
  where
    go _ [] = []
    go x0 (place : places) = 
      let rect = wpRectangle place
          x' = x0 - rect_width rect
          rect' = rect {rect_x = fi x'}
          place' = place {wpRectangle = rect'}
      in  place' : go x' places

alignCenter :: forall engine a. DecorationEngine engine a => engine a -> DrawData engine -> [Widget engine] -> X [WidgetPlace]
alignCenter engine dd widgets = do
    places <- alignLeft engine dd widgets
    let totalWidth = sum $ map (rect_width . wpRectangle) places
        availableWidth = fi (rect_width (ddDecoRect dd)) :: Position
        x0 = max 0 $ (availableWidth - fi totalWidth) `div` 2
        places' = map (shift x0) places
    return $ pack (fi availableWidth) places'
  where
    shift x0 place =
      let rect = wpRectangle place
          rect' = rect {rect_x = rect_x rect + fi x0}
      in  place {wpRectangle = rect'}
    
    pack _ [] = []
    pack available (place : places) =
      let rect = wpRectangle place
          placeWidth = rect_width rect
          widthToUse = min available placeWidth
          remaining = available - widthToUse
          rect' = rect {rect_width = widthToUse}
          place' = place {wpRectangle = rect'}
      in  place' : pack remaining places

mkDrawData :: (DecorationEngine engine a, Shrinker shrinker, ThemeAttributes (ThemeW engine), HasWidgets (Theme engine) (Widget engine))
           => engine a -> shrinker -> ThemeW engine -> DecorationEngineState engine -> Window -> Rectangle -> X (DrawData engine)
mkDrawData engine shrinker theme decoState origWindow decoRect@(Rectangle _ _ wh ht) = do
    -- xmonad-contrib #809
    -- qutebrowser will happily shovel a 389K multiline string into @_NET_WM_NAME@
    -- and the 'defaultShrinker' (a) doesn't handle multiline strings well (b) is
    -- quadratic due to using 'init'
    name  <- fmap (take 2048 . takeWhile (/= '\n') . show) (getName origWindow)
    style <- selectWindowStyle theme origWindow
    return $ DrawData {
                   ddStyleState = decoState,
                   ddStyle = style,
                   ddOrigWindow = origWindow,
                   ddWindowTitle = name,
                   ddDecoRect = decoRect,
                   ddLabels = themeWidgets theme,
                   ddWidgetPlaces = WidgetLayout [] [] []
                  }

genericWindowStyle :: Window -> GenericTheme style widget -> X style
genericWindowStyle win theme = do
  styleType <- windowStyleType win
  return $ case styleType of
             ActiveWindow -> exActive theme
             InactiveWindow -> exInactive theme
             UrgentWindow -> exUrgent theme

windowStyleType :: Window -> X ThemeStyleType
windowStyleType win = do
  mbFocused <- W.peek <$> gets windowset
  isWmStateUrgent <- (win `elem`) <$> readUrgents
  isUrgencyBitSet <- withDisplay $ \dpy -> do
                       hints <- io $ getWMHints dpy win
                       return $ wmh_flags hints `testBit` urgencyHintBit
  if isWmStateUrgent || isUrgencyBitSet
    then return UrgentWindow
    else return $
      case mbFocused of
        Nothing -> InactiveWindow
        Just focused
          | focused == win -> ActiveWindow
          | otherwise -> InactiveWindow

-- | Mouse focus and mouse drag are handled by the same function, this
-- way we can start dragging unfocused windows too.
handleMouseFocusDrag :: (DecorationEngine engine a, Shrinker shrinker) => engine a -> ThemeW engine -> DecorationLayoutState engine -> shrinker -> Event -> X ()
handleMouseFocusDrag ds theme (DecorationLayoutState {dsDecorations}) shrinker (ButtonEvent {ev_window, ev_x_root, ev_y_root, ev_event_type, ev_button})
    | ev_event_type == buttonPress
    , Just (WindowDecoration {..}) <- findDecoDataByDecoWindow ev_window dsDecorations = do
        let decoRect@(Rectangle dx dy _ _) = fromJust wdDecoRect
            x = fi $ ev_x_root - fi dx
            y = fi $ ev_y_root - fi dy
            button = fi ev_button
        dealtWith <- handleDecorationClick ds theme decoRect (map wpRectangle wdWidgets) wdOrigWindow x y button
        unless dealtWith $ when (isDraggingEnabled theme button) $
            mouseDrag (\x y -> focus wdOrigWindow >> decorationWhileDraggingHook ds ev_x_root ev_y_root (wdOrigWindow, wdOrigWinRect) x y)
                      (decorationAfterDraggingHook ds (wdOrigWindow, wdOrigWinRect) ev_window)
handleMouseFocusDrag _ _ _ _ _ = return ()

-- | Given a window and the state, if a matching decoration is in the
-- state return it with its ('Maybe') 'Rectangle'.
findDecoDataByDecoWindow :: Window -> [WindowDecoration] -> Maybe WindowDecoration
findDecoDataByDecoWindow decoWin = find (\dd -> wdDecoWindow dd == Just decoWin)

decorationHandler :: forall engine a.
                     (DecorationEngine engine a,
                      ClickHandler (Theme engine) (Widget engine))
                  => engine a
                  -> ThemeW engine
                  -> Rectangle
                  -> [Rectangle]
                  -> Window
                  -> Int
                  -> Int
                  -> Int
                  -> X Bool
decorationHandler deco theme decoRect widgetPlaces window x y button = do
    widgetDone <- go $ zip (widgetLayout $ themeWidgets theme) widgetPlaces
    if widgetDone
      then return True
      else case onDecorationClick theme button of
             Just cmd -> do
               executeWindowCommand cmd window
               return True
             Nothing -> return False
  where
    go :: [(Widget engine, Rectangle)] -> X Bool
    go [] = return False
    go ((w, rect) : rest) = do
      if pointWithin (fi x) (fi y) rect
        then do
          executeWindowCommand (widgetCommand w button) window
          return True
        else go rest

defaultPaintDecoration :: forall engine shrinker.
                          (DecorationEngine engine Window,
                           DecorationPaintingContext engine ~ XPaintingContext,
                           Shrinker shrinker,
                           Style (Theme engine (Widget engine)) ~ SimpleStyle)
                       => engine Window
                       -> Window
                       -> Dimension
                       -> Dimension
                       -> shrinker
                       -> DrawData engine
                       -> X ()
defaultPaintDecoration deco win windowWidth windowHeight shrinker dd = do
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
    forM_ (zip widgets $ widgetLayout $ ddWidgetPlaces dd) $ \(widget, place) ->
        paintWidget deco (dpy, pixmap, gc) place shrinker dd widget

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

defaultPlaceWidgets :: (Shrinker shrinker, DecorationEngine engine a, ThemeAttributes (ThemeW engine))
                    => engine a -> ThemeW engine -> shrinker -> DecorationEngineState engine -> Rectangle -> Window -> WidgetLayout (Widget engine) -> X (WidgetLayout WidgetPlace)
defaultPlaceWidgets engine theme shrinker decoStyle decoRect window wlayout = do
    let leftWidgets = wlLeft wlayout
        rightWidgets = wlRight wlayout
        centerWidgets = wlCenter wlayout

    dd <- mkDrawData engine shrinker theme decoStyle window decoRect
    let dd' = dd {ddDecoRect = pad (widgetsPadding theme) (ddDecoRect dd)}
    rightRects <- alignRight engine dd' rightWidgets
    leftRects <- alignLeft engine dd' leftWidgets
    let leftWidgetsWidth = sum $ map (rect_width . wpRectangle) leftRects
        rightWidgetsWidth = sum $ map (rect_width . wpRectangle) rightRects
        dd'' = dd' {ddDecoRect = padCenter leftWidgetsWidth rightWidgetsWidth (ddDecoRect dd')}
    centerRects <- alignCenter engine dd'' centerWidgets
    return $ WidgetLayout leftRects centerRects rightRects
  where
    pad p (Rectangle x y w h) =
      Rectangle (fi (bxLeft p)) (fi (bxTop p))
                (w - bxLeft p - bxRight p)
                (h - bxTop p - bxBottom p)
  
    padCenter left right (Rectangle x y w h) =
      Rectangle (x + fi left) y
                (w - left - right) h

