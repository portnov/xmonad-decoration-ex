{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module XMonad.Layout.DecorationEx.DwmTextDecoration where 

import XMonad
import XMonad.Layout.LayoutModifier
import qualified XMonad.Layout.Decoration as D
import qualified XMonad.Layout.DwmStyle as Dwm

import XMonad.Layout.DecorationEx.LayoutModifier
import XMonad.Layout.DecorationEx.Types
import XMonad.Layout.DecorationEx.DecorationStyleEx
import XMonad.Layout.DecorationEx.Widgets
import XMonad.Layout.DecorationEx.TextDecoration

data DwmTextDecoration a = DwmTextDecoration
  deriving (Show, Read)

instance DecorationStyleEx DwmTextDecoration Window where
  type Theme DwmTextDecoration = ThemeEx
  type Widget DwmTextDecoration = StandardWidget

  describeDecoration DwmTextDecoration = "DwmText"

  pureDecoration DwmTextDecoration theme screenRect stack wrs (w, r) =
    let (wh, ht) = decorationSize theme
    in  D.pureDecoration Dwm.Dwm wh ht screenRect stack wrs (w, r)

  shrinkWindow _ _ r = r

  placeWidgets = defaultPlaceWidgets
  paintDecoration = defaultPaintDecoration
  paintWidget = paintTextWidget
  calcWidgetPlace = calcTextWidgetPlace

dwmStyleDeco :: D.Shrinker shrinker => shrinker -> ThemeEx StandardWidget -> l Window
             -> ModifiedLayout (DecorationEx DwmTextDecoration shrinker) l Window
dwmStyleDeco s theme = decorationEx s theme DwmTextDecoration

