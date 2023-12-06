{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module XMonad.Layout.DecorationEx.DwmTextDecoration where 

import XMonad
import qualified XMonad.StackSet as W
import XMonad.Layout.LayoutModifier
import qualified XMonad.Layout.Decoration as D
import XMonad.Util.Font

import XMonad.Layout.DecorationEx.LayoutModifier
import XMonad.Layout.DecorationEx.Types
import XMonad.Layout.DecorationEx.DecorationStyleEx
import XMonad.Layout.DecorationEx.Widgets
import XMonad.Layout.DecorationEx.TextDecoration

data DwmTextDecoration a = DwmTextDecoration Bool
  deriving (Show, Read)

instance DecorationStyleEx DwmTextDecoration Window where
  type Theme DwmTextDecoration = GenericTheme SimpleStyle
  type Widget DwmTextDecoration = StandardWidget
  type DecorationPaintingContext DwmTextDecoration = XPaintingContext
  type DecorationStyleState DwmTextDecoration = XMonadFont

  describeDecoration (DwmTextDecoration _) = "DwmText"

  initializeState dstyle theme = initXMF (themeFontName theme)
  releaseStateResources dstyle = releaseXMF

  pureDecoration (DwmTextDecoration showForFocused) theme screenRect stack wrs (w, Rectangle x y windowWidth windowHeight) =
    let (decoWidth, decoHeight) = decorationSize theme
        nwh = min windowWidth $ fromIntegral decoWidth
        nx = fromIntegral x + windowWidth - nwh
        focusedWindow = W.focus stack
        isFocused = focusedWindow == w
    in  if (not showForFocused && isFocused) || not (D.isInStack stack w)
          then Nothing
          else Just $ Rectangle (fromIntegral nx) y nwh (fromIntegral decoHeight)

  shrinkWindow _ _ r = r

  placeWidgets = defaultPlaceWidgets
  paintDecoration = defaultPaintDecoration
  paintWidget = paintTextWidget
  calcWidgetPlace = calcTextWidgetPlace

dwmStyleDeco :: D.Shrinker shrinker => shrinker -> ThemeEx StandardWidget -> Bool
             -> l Window
             -> ModifiedLayout (DecorationEx DwmTextDecoration shrinker) l Window
dwmStyleDeco s theme showForFocused = decorationEx s theme (DwmTextDecoration showForFocused)

