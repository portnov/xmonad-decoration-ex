{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module XMonad.Layout.DecorationEx.Types (
    WindowDecoration (..)
  , DecorationStateEx (..)
  , WindowCommand (..)
  , DecorationWidget (..)
  , WidgetPlace (..)
  , WidgetLayout (..)
  , HasWidgets (..)
  , ThemeAttributes (..)
  , DecorationPaintingContext
  , BoxBorders (..), BorderColors, borderColor, shadowBorder
  , SimpleStyle (..), ThemeEx (..), themeEx
  , widgetLayout
  ) where

import Control.Monad
import Data.Default
import qualified Data.Map as M
import Foreign.C.Types (CInt)

import XMonad
import XMonad.Prelude
import qualified XMonad.StackSet as W
import XMonad.Layout.Decoration (DecorationMsg (..), Shrinker (..))
import qualified XMonad.Layout.Decoration as D
import XMonad.Layout.WindowArranger (WindowArrangerMsg (..), diff, listFromList)
import XMonad.Layout.DraggingVisualizer (DraggingVisualizerMsg (..))
import XMonad.Util.Font
import XMonad.Util.Invisible
import XMonad.Util.XUtils hiding (paintTextAndIcons)
import XMonad.Util.Image
import XMonad.Util.NamedWindows (getName)
import XMonad.Hooks.UrgencyHook

data WindowDecoration = WindowDecoration {
    wdOrigWindow :: Window
  , wdOrigWinRect :: Rectangle
  , wdDecoWindow :: Maybe Window
  , wdDecoRect :: Maybe Rectangle
  , wdWidgets :: [WidgetPlace]
  }

data DecorationStateEx = DecorationStateEx {
    dsFont :: XMonadFont
  , dsDecorations :: [WindowDecoration]
  }

class (Read cmd, Show cmd) => WindowCommand cmd where
  executeWindowCommand :: cmd -> Window -> X ()
  isCommandChecked :: cmd -> Window -> X Bool

class (WindowCommand (WidgetCommand widget), Read widget, Show widget)
  => DecorationWidget widget where
  type WidgetCommand widget
  widgetCommand :: widget -> WidgetCommand widget

data WidgetLayout a = WidgetLayout {
    wlLeft :: [a]
  , wlCenter :: [a]
  , wlRight :: [a]
  }

data WidgetPlace = WidgetPlace {
    wpTextYPosition :: Position
  , wpRectangle :: Rectangle
  }
  deriving (Show)

data BoxBorders a = BoxBorders {
    bxTop :: a
  , bxRight :: a
  , bxBottom :: a
  , bxLeft :: a
  } deriving (Eq, Read, Show)

type BorderColors = BoxBorders String

borderColor :: String -> BorderColors
borderColor c = BoxBorders c c c c

shadowBorder :: String -> String -> BorderColors
shadowBorder highlight shadow = BoxBorders highlight shadow shadow highlight

data SimpleStyle = SimpleStyle {
    sBgColor :: String
  , sBorderColor :: String
  , sTextColor :: String
  , sTextBgColor :: String
  , sDecoBorderWidth :: Dimension
  , sDecorationBorders :: BorderColors
  }
  deriving (Show, Read)

class HasWidgets theme widget where
  themeWidgets :: theme widget -> WidgetLayout widget

class (Read theme, Show theme) => ThemeAttributes theme where
  type Style theme
  selectWindowStyle :: theme -> Window -> X (Style theme)
  widgetsPadding :: theme -> BoxBorders Dimension
  defaultBgColor :: theme -> String
  decorationSize :: theme -> (Dimension, Dimension)
  themeFontName :: theme -> String

data ThemeEx widget = ThemeEx {
    exActive :: SimpleStyle
  , exInactive :: SimpleStyle
  , exUrgent :: SimpleStyle
  , exPadding :: BoxBorders Dimension
  , exFontName :: String
  , exDecoWidth :: Dimension
  , exDecoHeight :: Dimension
  , exWidgetsLeft :: [widget]
  , exWidgetsCenter :: [widget]
  , exWidgetsRight :: [widget]
  }
  deriving (Read, Show)

instance HasWidgets ThemeEx widget where
  themeWidgets theme = WidgetLayout (exWidgetsLeft theme) (exWidgetsCenter theme) (exWidgetsRight theme)

themeEx :: D.Theme -> ThemeEx widget
themeEx t =
    ThemeEx {
          exActive = SimpleStyle (D.activeColor t) (D.activeBorderColor t) (D.activeTextColor t) (D.activeColor t) (D.activeBorderWidth t) (borderColor $ D.activeColor t)
        , exInactive = SimpleStyle (D.inactiveColor t) (D.inactiveBorderColor t) (D.inactiveTextColor t) (D.inactiveColor t) (D.inactiveBorderWidth t) (borderColor $ D.inactiveColor t)
        , exUrgent = SimpleStyle (D.urgentColor t) (D.urgentBorderColor t) (D.urgentTextColor t) (D.urgentColor t) (D.urgentBorderWidth t) (borderColor $ D.urgentColor t)
        , exPadding = BoxBorders 0 4 0 4
        , exFontName = D.fontName t
        , exDecoWidth = D.decoWidth t
        , exDecoHeight = D.decoHeight t
        , exWidgetsLeft = []
        , exWidgetsCenter = []
        , exWidgetsRight = []
      }

instance Default (ThemeEx widget) where
  def = themeEx (def :: D.Theme)

widgetLayout :: WidgetLayout widget -> [widget]
widgetLayout ws = wlLeft ws ++ wlCenter ws ++ wlRight ws

type DecorationPaintingContext = (Display, Pixmap, GC)

