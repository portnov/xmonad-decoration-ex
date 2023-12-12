{-# LANGUAGE FlexibleInstances #-}
module XMonad.Layout.DecorationEx.Theme where

import Data.Default
import qualified Data.Map as M

import qualified XMonad.Layout.Decoration as D

import XMonad.Layout.DecorationEx.Types
import XMonad.Layout.DecorationEx.Widgets

themeEx :: D.Theme -> ThemeEx StandardWidget
themeEx t =
    GenericTheme {
          exActive = SimpleStyle (D.activeColor t) (D.activeBorderColor t) (D.activeTextColor t) (D.activeColor t) (D.activeBorderWidth t) (borderColor $ D.activeColor t)
        , exInactive = SimpleStyle (D.inactiveColor t) (D.inactiveBorderColor t) (D.inactiveTextColor t) (D.inactiveColor t) (D.inactiveBorderWidth t) (borderColor $ D.inactiveColor t)
        , exUrgent = SimpleStyle (D.urgentColor t) (D.urgentBorderColor t) (D.urgentTextColor t) (D.urgentColor t) (D.urgentBorderWidth t) (borderColor $ D.urgentColor t)
        , exPadding = BoxBorders 0 4 0 4
        , exFontName = D.fontName t
        , exOnDecoClick = M.fromList [(1, FocusWindow)]
        , exDragWindowButtons = [1]
        , exWidgetsLeft = []
        , exWidgetsCenter = []
        , exWidgetsRight = []
      }

instance Default (ThemeEx StandardWidget) where
  def = themeEx (def :: D.Theme)

