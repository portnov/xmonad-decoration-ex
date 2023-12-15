{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module XMonad.Layout.DecorationEx.Types (
    WindowDecoration (..)
  , WindowCommand (..)
  , DecorationWidget (..)
  , WidgetPlace (..)
  , WidgetLayout (..)
  , HasWidgets (..)
  , ClickHandler (..)
  , ThemeAttributes (..)
  , XPaintingContext
  , BoxBorders (..), BorderColors, borderColor, shadowBorder
  , ThemeStyleType (..)
  , SimpleStyle (..), GenericTheme (..), ThemeEx 
  , widgetLayout
  ) where

import qualified Data.Map as M

import XMonad

-- | Information about decoration of one window
data WindowDecoration = WindowDecoration {
    wdOrigWindow :: !Window         -- ^ Original window (one being decorated)
  , wdOrigWinRect :: !Rectangle     -- ^ Rectangle of original window
  , wdDecoWindow :: !(Maybe Window) -- ^ Decoration window, or Nothing if this window should not be decorated
  , wdDecoRect :: !(Maybe Rectangle) -- ^ Rectangle for decoration window
  , wdWidgets :: ![WidgetPlace]      -- ^ Places for widgets
  }

-- | Type class for window commands (such as maximize or close window)
class (Read cmd, Show cmd) => WindowCommand cmd where
  -- | Execute the command
  executeWindowCommand :: cmd -> Window -> X Bool

  -- | Is the command currently in `checked' state. 
  -- For example, for 'sticky' command, check if the
  -- window is currently sticky.
  isCommandChecked :: cmd -> Window -> X Bool

-- | Type class for decoration widgets
class (WindowCommand (WidgetCommand widget), Read widget, Show widget)
  => DecorationWidget widget where
  -- | Type of window commands which this type of widgets can execute
  type WidgetCommand widget

  -- | Get window command which is associated with this widget.
  widgetCommand :: widget -> Int -> WidgetCommand widget

  -- | Check if the widget is shrinkable, i.e. if it's width
  -- can be reduced if there is not enough place in the decoration.
  isShrinkable :: widget -> Bool

-- | Layout of widgets
data WidgetLayout a = WidgetLayout {
    wlLeft :: ![a]     -- ^ Widgets that should be aligned to the left side of decoration
  , wlCenter :: ![a]   -- ^ Widgets that should be in the center of decoration
  , wlRight :: ![a]    -- ^ Widgets taht should be aligned to the right side of decoration
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

class ClickHandler theme widget where
  onDecorationClick :: theme widget -> Int -> Maybe (WidgetCommand widget)
  isDraggingEnabled :: theme widget -> Int -> Bool

class (Read theme, Show theme) => ThemeAttributes theme where
  type Style theme
  selectWindowStyle :: theme -> Window -> X (Style theme)
  widgetsPadding :: theme -> BoxBorders Dimension
  defaultBgColor :: theme -> String
  themeFontName :: theme -> String

data GenericTheme style widget = GenericTheme {
    exActive :: !style
  , exInactive :: !style
  , exUrgent :: !style
  , exPadding :: !(BoxBorders Dimension)
  , exFontName :: !String
  , exOnDecoClick :: !(M.Map Int (WidgetCommand widget))
  , exDragWindowButtons :: ![Int]
  , exWidgetsLeft :: ![widget]
  , exWidgetsCenter :: ![widget]
  , exWidgetsRight :: ![widget]
  }

deriving instance (Show widget, Show (WidgetCommand widget), Show style) => Show (GenericTheme style widget)
deriving instance (Read widget, Read (WidgetCommand widget), Read style) => Read (GenericTheme style widget)

type ThemeEx widget = GenericTheme SimpleStyle widget

instance HasWidgets (GenericTheme style) widget where
  themeWidgets theme = WidgetLayout (exWidgetsLeft theme) (exWidgetsCenter theme) (exWidgetsRight theme)

data ThemeStyleType = ActiveWindow | UrgentWindow | InactiveWindow
  deriving (Eq, Show, Read)

widgetLayout :: WidgetLayout widget -> [widget]
widgetLayout ws = wlLeft ws ++ wlCenter ws ++ wlRight ws

type XPaintingContext = (Display, Pixmap, GC)

