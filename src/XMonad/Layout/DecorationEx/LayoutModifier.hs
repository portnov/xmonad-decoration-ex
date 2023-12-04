{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module XMonad.Layout.DecorationEx.LayoutModifier where

import Control.Monad
import Data.Default
import qualified Data.Map as M
import Data.Bits (testBit)
import Foreign.C.Types (CInt)

import XMonad
import XMonad.Prelude
import qualified XMonad.StackSet as W
import XMonad.Layout.LayoutModifier
import XMonad.Layout.Decoration (Theme (..), DecorationMsg (..), Shrinker (..), shrinkWhile)
import XMonad.Layout.WindowArranger (WindowArrangerMsg (..), diff, listFromList)
import XMonad.Layout.DraggingVisualizer (DraggingVisualizerMsg (..))
import XMonad.Util.Font
import XMonad.Util.Invisible
import XMonad.Util.XUtils hiding (paintTextAndIcons)
import XMonad.Util.Image
import XMonad.Util.NamedWindows (getName)
import XMonad.Hooks.UrgencyHook

import XMonad.Layout.DecorationEx.Types
import XMonad.Layout.DecorationEx.DecorationStyleEx
import XMonad.Layout.DecorationEx.Widgets

-- | The 'Decoration' 'LayoutModifier'. This data type is an instance
-- of the 'LayoutModifier' class. This data type will be passed,
-- together with a layout, to the 'ModifiedLayout' type constructor
-- to modify the layout by adding decorations according to a
-- 'DecorationStyle'.
data DecorationEx dstyle shrinker a =
    DecorationEx (Invisible Maybe DecorationStateEx) shrinker (ThemeW dstyle) (dstyle a)

deriving instance (Show (ThemeW dstyle), Show shrinker, Show (dstyle a)) => Show (DecorationEx dstyle shrinker a)
deriving instance (Read (ThemeW dstyle), Read shrinker, Read (dstyle a)) => Read (DecorationEx dstyle shrinker a)

-- | The long 'LayoutModifier' instance for the 'Decoration' type.
--
-- In 'redoLayout' we check the state: if there is no state we
-- initialize it.
--
-- The state is 'diff'ed against the list of windows produced by the
-- underlying layout: removed windows get deleted and new ones
-- decorated by 'createDecos', which will call 'decorate' to decide if
-- a window must be given a 'Rectangle', in which case a decoration
-- window will be created.
--
-- After that we resync the updated state with the windows' list and
-- then we process the resynced stated (as we do with a new state).
--
-- First we map the decoration windows, we update each decoration to
-- reflect any decorated window's change, and we insert, in the list
-- of windows and rectangles returned by the underlying layout, the
-- decoration for each window. This way xmonad will restack the
-- decorations and their windows accordingly. At the end we remove
-- invisible\/stacked windows.
--
-- Message handling is quite simple: when needed we release the state
-- component of the 'Decoration' 'LayoutModifier'. Otherwise we call
-- 'handleEvent', which will call the appropriate 'DecorationStyle'
-- methods to perform its tasks.
instance (DecorationStyleEx dstyle Window, Shrinker shrinker) => LayoutModifier (DecorationEx dstyle shrinker) Window where
    redoLayout (DecorationEx (I (Just state)) shrinker theme dstyle) _ Nothing _ = do
        releaseResources state
        return ([], Just $ DecorationEx (I Nothing) shrinker theme dstyle)
    redoLayout _ _ Nothing _  = return ([], Nothing)

    redoLayout (DecorationEx st shrinker theme dstyle) screenRect (Just stack) srcPairs
        | I Nothing  <- st = initState theme dstyle shrinker screenRect stack srcPairs >>= processState
        | I (Just s) <- st = do
            let decorations  = dsDecorations s
                (d,a) = curry diff (getOrigWindows decorations) srcWindows
                toDel = todel d decorations
                toAdd = toadd a srcPairs
            deleteDecos toDel
            let decosToBeAdded = [WindowDecoration win rect Nothing Nothing [] | (win, rect) <- toAdd]
            newDecorations <- resync (dsFont s) (decosToBeAdded ++ del_dwrs d decorations) srcPairs
            processState (s {dsDecorations = newDecorations})

        where
          srcWindows = map fst srcPairs

          getOrigWindows :: [WindowDecoration] -> [Window]
          getOrigWindows = map wdOrigWindow

          del_dwrs :: [Window] -> [WindowDecoration] -> [WindowDecoration]
          del_dwrs = listFromList wdOrigWindow notElem

          findDecoWindow :: Int -> [WindowDecoration] -> Maybe Window
          findDecoWindow i d = wdDecoWindow $ d !! i

          todel :: [Window] -> [WindowDecoration] -> [WindowDecoration]
          todel d = filter (\dd -> wdOrigWindow dd `elem` d)

          toadd :: [Window] -> [(Window, Rectangle)] -> [(Window, Rectangle)]
          toadd a = filter (\p -> fst p `elem` a)

          createDecoWindowIfNeeded :: Maybe Window -> Maybe Rectangle -> X (Maybe Window)
          createDecoWindowIfNeeded mbDecoWindow mbDecoRect =
            case (mbDecoWindow, mbDecoRect) of
              (Nothing, Just decoRect) -> do
                decoWindow <- createDecoWindow theme decoRect
                return $ Just decoWindow
              _ -> return mbDecoWindow

          resync :: XMonadFont -> [WindowDecoration] -> [(Window,Rectangle)] -> X [WindowDecoration]
          resync _ _ [] = return []
          resync font dd ((window,rect):xs) =
            case  window `elemIndex` getOrigWindows dd of
              Just i  -> do
                mbDecoRect <- decorateWindow dstyle theme screenRect stack srcPairs (window,rect)
                widgetPlaces <- case mbDecoRect of
                                  Nothing -> return []
                                  Just decoRect -> placeWidgets dstyle theme shrinker font decoRect window (themeWidgets theme)
                mbDecoWindow  <- createDecoWindowIfNeeded (findDecoWindow i dd) mbDecoRect
                let newDd = WindowDecoration window rect mbDecoWindow mbDecoRect widgetPlaces
                restDd <- resync font dd xs
                return $ newDd : restDd
              Nothing -> resync font dd xs

          -- We drop any windows that are *precisely* stacked underneath
          -- another window: these must be intended to be tabbed!
          removeTabbed :: [Rectangle] -> [(Window, Rectangle)] -> [(Window, Rectangle)]
          removeTabbed _ [] = []
          removeTabbed rs ((w,r):xs)
              | r `elem` rs = removeTabbed rs xs
              | otherwise   = (w,r) : removeTabbed (r:rs) xs

          insertDwr :: WindowDecoration -> [(Window, Rectangle)] -> [(Window, Rectangle)]
          insertDwr dd wrs =
            case (wdDecoWindow dd, wdDecoRect dd) of
              (Just decoWindow, Just decoRect) -> (decoWindow, decoRect) : (wdOrigWindow dd, shrinkWindow dstyle decoRect (wdOrigWinRect dd)) : wrs
              _ -> (wdOrigWindow dd, wdOrigWinRect dd) : wrs

          dwrs_to_wrs :: [WindowDecoration] -> [(Window, Rectangle)]
          dwrs_to_wrs = removeTabbed [] . foldr insertDwr []

          processState :: DecorationStateEx -> X ([(Window, Rectangle)], Maybe (DecorationEx dstyle shrinker Window))
          processState st = do
            let decorations = dsDecorations st
            showDecos decorations
            updateDecos dstyle shrinker theme (dsFont st) decorations
            return (dwrs_to_wrs decorations, Just (DecorationEx (I (Just (st {dsDecorations = decorations}))) shrinker theme dstyle))

    handleMess (DecorationEx (I (Just st)) shrinker theme dstyle) m
        | Just e <- fromMessage m = do
            decorationEventHookEx dstyle theme st shrinker e
            handleEvent dstyle shrinker theme st e
            return Nothing
        | Just Hide <- fromMessage m = do
            hideDecos $ dsDecorations st
            return Nothing
--         | Just (SetTheme nt) <- fromMessage m = do
--             releaseResources st
--             let t' = themeEx nt
--             return $ Just $ DecorationEx (I Nothing) shrinker t' dstyle
        | Just ReleaseResources <- fromMessage m = do
            releaseResources st
            return $ Just $ DecorationEx (I Nothing) shrinker theme  dstyle
    handleMess _ _ = return Nothing

    modifierDescription (DecorationEx _ _ _ dstyle) = describeDecoration dstyle

-- | By default 'Decoration' handles 'PropertyEvent' and 'ExposeEvent'
-- only.
handleEvent :: (Shrinker shrinker, DecorationStyleEx dstyle Window) => dstyle Window -> shrinker -> ThemeW dstyle -> DecorationStateEx -> Event -> X ()
handleEvent dstyle shrinker theme (DecorationStateEx {..}) e
    | PropertyEvent {ev_window = w} <- e
    , Just i <- w `elemIndex` map wdOrigWindow dsDecorations = updateDeco dstyle shrinker theme dsFont (dsDecorations !! i)
    | ExposeEvent   {ev_window = w} <- e
    , Just i <- w `elemIndex` mapMaybe wdDecoWindow dsDecorations = updateDeco dstyle shrinker theme dsFont (dsDecorations !! i)
handleEvent _ _ _ _ _ = return ()

-- | Initialize the 'DecorationState' by initializing the font
-- structure and by creating the needed decorations.
initState :: (DecorationStyleEx dstyle Window, Shrinker shrinker)
          => ThemeW dstyle
          -> dstyle Window
          -> shrinker
          -> Rectangle
          -> W.Stack Window
          -> [(Window,Rectangle)] -> X DecorationStateEx
initState theme dstyle shrinker screenRect stack wrs = do
  font <- initXMF (themeFontName theme)
  decorations <- createDecos theme dstyle shrinker font screenRect stack wrs wrs
  return $ DecorationStateEx font decorations

-- | Delete windows stored in the state and release the font structure.
releaseResources :: DecorationStateEx -> X ()
releaseResources st = do
  deleteDecos (dsDecorations st)
  releaseXMF  (dsFont st)

-- | Create the decoration windows of a list of windows and their
-- rectangles, by calling the 'decorate' method of the
-- 'DecorationStyle' received.
createDecos :: (DecorationStyleEx dstyle Window, Shrinker shrinker)
            => ThemeW dstyle
            -> dstyle Window
            -> shrinker
            -> XMonadFont
            -> Rectangle
            -> W.Stack Window
            -> [(Window,Rectangle)] -> [(Window,Rectangle)] -> X [WindowDecoration]
createDecos theme dstyle shrinker font screenRect stack wrs ((w,r):xs) = do
  mbDecoRect <- decorateWindow dstyle theme screenRect stack wrs (w,r)
  case mbDecoRect of
    Just decoRect -> do
      decoWindow <- createDecoWindow theme decoRect
      widgetPlaces <- placeWidgets dstyle theme shrinker font decoRect w (themeWidgets theme)
      restDd <- createDecos theme dstyle shrinker font screenRect stack wrs xs
      let newDd = WindowDecoration w r (Just decoWindow) (Just decoRect) widgetPlaces
      return $ newDd : restDd
    Nothing -> do
      restDd <- createDecos theme dstyle shrinker font screenRect stack wrs xs
      let newDd = WindowDecoration w r Nothing Nothing []
      return $ newDd : restDd
createDecos _ _ _ _ _ _ _ [] = return []

createDecoWindow :: ThemeAttributes theme => theme -> Rectangle -> X Window
createDecoWindow theme rect = do
  let mask = Just (exposureMask .|. buttonPressMask)
  w <- createNewWindow rect mask (defaultBgColor theme) True
  d <- asks display
  io $ setClassHint d w (ClassHint "xmonad-decoration" "xmonad")
  return w

showDecos :: [WindowDecoration] -> X ()
showDecos dd =
  showWindows $ mapMaybe wdDecoWindow $ filter (isJust . wdDecoRect) dd

hideDecos :: [WindowDecoration] -> X ()
hideDecos = hideWindows . mapMaybe wdDecoWindow

deleteDecos :: [WindowDecoration] -> X ()
deleteDecos = deleteWindows . mapMaybe wdDecoWindow

updateDecos :: (Shrinker shrinker, DecorationStyleEx dstyle Window)
            => dstyle Window -> shrinker -> ThemeW dstyle -> XMonadFont -> [WindowDecoration] -> X ()
updateDecos dstyle shrinker theme font = mapM_ $ updateDeco dstyle shrinker theme font

withFont :: String -> (XMonadFont -> X()) -> X ()
withFont fontName actions = do
  font <- initXMF fontName
  result <- actions font
  releaseXMF font
  return result

-- | Update a decoration window given a shrinker, a theme, the font
-- structure and the needed 'Rectangle's
updateDeco :: (Shrinker shrinker, DecorationStyleEx dstyle Window) => dstyle Window -> shrinker -> ThemeW dstyle -> XMonadFont -> WindowDecoration -> X ()
updateDeco dstyle shrinker theme font wd =
  case (wdDecoWindow wd, wdDecoRect wd) of
    (Just decoWindow, Just decoRect@(Rectangle _ _ wh ht)) -> do
      let origWin = wdOrigWindow wd
      drawData <- mkDrawData shrinker theme font origWin decoRect
      widgetPlaces <- placeWidgets dstyle theme shrinker font decoRect (wdOrigWindow wd) (themeWidgets theme)
      -- io $ print widgetPlaces
      paintDecoration dstyle decoWindow wh ht $ drawData {ddWidgetPlaces = widgetPlaces}
    (Just decoWindow, Nothing) -> hideWindow decoWindow
    _ -> return ()

decorationEx :: (DecorationStyleEx dstyle a, Shrinker shrinker) => shrinker -> ThemeW dstyle -> dstyle a
           -> l a -> ModifiedLayout (DecorationEx dstyle shrinker) l a
decorationEx shrinker theme dstyle = ModifiedLayout (DecorationEx (I Nothing) shrinker theme dstyle)

