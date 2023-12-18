{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.DecorationEx.Types
-- Description :  Declaration of types used by DecorationEx module.
-- Copyright   :  (c) 2007 Andrea Rossato, 2009 Jan Vornberger, 2023 Ilya Portnov
-- License     :  BSD-style (see xmonad/LICENSE)
--
-- Maintainer  :  portnov84@rambler.ru
-- Stability   :  unstable
-- Portability :  unportable
--
-- This module exposes a number of types which are used by other sub-modules
-- of "XMonad.Layout.DecorationEx" module.
-----------------------------------------------------------------------------

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
  , BoxBorders (..), BorderColors
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

-- | Data type describing where the decoration widget (e.g. window button)
-- should be placed.
-- All coordinates are relative to decoration rectangle.
data WidgetPlace = WidgetPlace {
    wpTextYPosition :: !Position -- ^ Y position of text base line
                                 -- (for widgets like window title or text-based buttons)
  , wpRectangle :: !Rectangle    -- ^ Rectangle where to place the widget
  }
  deriving (Show)

-- | Generic data type which is used to
-- describe characteristics of rectangle borders.
data BoxBorders a = BoxBorders {
    bxTop :: !a
  , bxRight :: !a
  , bxBottom :: !a
  , bxLeft :: !a
  } deriving (Eq, Read, Show)

-- | Convinience data type describing colors of decoration rectangle borders.
type BorderColors = BoxBorders String

-- | Data type describing look of window decoration
-- in particular state (active or inactive)
data SimpleStyle = SimpleStyle {
    sBgColor :: !String                 -- ^ Decoration background color
  -- , sBorderColor :: !String             
  , sTextColor :: !String               -- ^ Text (foreground) color
  , sTextBgColor :: !String             -- ^ Text background color
  , sDecoBorderWidth :: !Dimension      -- ^ Width of border of decoration rectangle. Set to 0 to disable the border.
  , sDecorationBorders :: !BorderColors -- ^ Colors of borders of decoration rectangle.
  }
  deriving (Show, Read)

-- | Type class for themes, which claims that
-- the theme contains the list of widgets and their alignments.
class HasWidgets theme widget where
  themeWidgets :: theme widget -> WidgetLayout widget

-- | Type class for themes, which claims that
-- the theme can describe how the decoration should respond
-- to clicks on decoration itself (between widgets).
class ClickHandler theme widget where
  -- | This is called when the user clicks on the decoration rectangle
  -- (not on one of widgets).
  onDecorationClick :: theme widget
                    -> Int                          -- ^ Mouse button number
                    -> Maybe (WidgetCommand widget)

  -- | Determine if it is possible to drag window by it's decoration
  -- with mouse button.
  isDraggingEnabled :: theme widget
                    -> Int          -- ^ Mouse button number
                    -> Bool

-- | Type class for themes, which claims that the theme
-- is responsible for determining looks of decoration.
class (Read theme, Show theme) => ThemeAttributes theme where
  -- | Type which describes looks of decoration in one
  -- of window states (active, inactive, urgent, etc).
  type Style theme

  -- | Select style based on window state.
  selectWindowStyle :: theme -> Window -> X (Style theme)

  -- | Define padding between decoration rectangle and widgets.
  widgetsPadding :: theme -> BoxBorders Dimension
  
  -- | Initial background color of decoration rectangle.
  -- When decoration widget is created, it is initially filled
  -- with this color.
  defaultBgColor :: theme -> String

  -- | Font name defined in the theme.
  themeFontName :: theme -> String

-- | Generic Theme data type. This is used
-- by @TextEngine@ and can be used by other relatively
-- simple decoration engines.
data GenericTheme style widget = GenericTheme {
    exActive :: !style                                  -- ^ Decoration style for active (focused) windows
  , exInactive :: !style                                -- ^ Decoration style for inactive (unfocused) windows
  , exUrgent :: !style                                  -- ^ Decoration style for urgent windows
  , exPadding :: !(BoxBorders Dimension)                -- ^ Padding between decoration rectangle and widgets
  , exFontName :: !String                               -- ^ Font name
  , exOnDecoClick :: !(M.Map Int (WidgetCommand widget)) -- ^ Correspondence between mouse button number and window command.
  , exDragWindowButtons :: ![Int]                       -- ^ For which mouse buttons dragging is enabled
  , exWidgetsLeft :: ![widget]                          -- ^ Widgets that should appear at the left of decoration rectangle (listed left to right)
  , exWidgetsCenter :: ![widget]                        -- ^ Widgets that should appear in the center of decoration rectangle (listed left to right)
  , exWidgetsRight :: ![widget]                         -- ^ Widgets that should appear at the right of decoration rectangle (listed left to right)
  }

deriving instance (Show widget, Show (WidgetCommand widget), Show style) => Show (GenericTheme style widget)
deriving instance (Read widget, Read (WidgetCommand widget), Read style) => Read (GenericTheme style widget)

-- | Convience type for themes used by @TextDecoration@.
type ThemeEx widget = GenericTheme SimpleStyle widget

instance HasWidgets (GenericTheme style) widget where
  themeWidgets theme = WidgetLayout (exWidgetsLeft theme) (exWidgetsCenter theme) (exWidgetsRight theme)

-- | Supported states of windows (on which looks of decorations can depend).
data ThemeStyleType = ActiveWindow | UrgentWindow | InactiveWindow
  deriving (Eq, Show, Read)

-- | Utility function to convert WidgetLayout to plain list of widgets.
widgetLayout :: WidgetLayout widget -> [widget]
widgetLayout ws = wlLeft ws ++ wlCenter ws ++ wlRight ws

-- | Painting context for decoration engines based on plain X11 calls.
type XPaintingContext = (Display, Pixmap, GC)

