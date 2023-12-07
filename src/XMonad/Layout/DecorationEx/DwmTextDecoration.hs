{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module XMonad.Layout.DecorationEx.DwmTextDecoration where 

import XMonad
import qualified XMonad.StackSet as W
import XMonad.Layout.LayoutModifier
import qualified XMonad.Layout.Decoration as D

import XMonad.Layout.DecorationEx.LayoutModifier
import XMonad.Layout.DecorationEx.Types
import XMonad.Layout.DecorationEx.DecorationStyleEx
import XMonad.Layout.DecorationEx.Widgets
import XMonad.Layout.DecorationEx.TextDecoration

newtype DwmGeometry a = DwmGeometry Bool
  deriving (Show, Read)

instance DecorationGeometry DwmGeometry Window where
  describeGeometry _ = "DwmText"

  pureDecoration (DwmGeometry showForFocused) (decoWidth, decoHeight) screenRect stack wrs (w, Rectangle x y windowWidth windowHeight) =
    let nwh = min windowWidth $ fromIntegral decoWidth
        nx = fromIntegral x + windowWidth - nwh
        focusedWindow = W.focus stack
        isFocused = focusedWindow == w
    in  if (not showForFocused && isFocused) || not (D.isInStack stack w)
          then Nothing
          else Just $ Rectangle (fromIntegral nx) y nwh (fromIntegral decoHeight)

  shrinkWindow _ _ r = r

dwmStyleDeco :: D.Shrinker shrinker => shrinker -> ThemeEx StandardWidget -> Bool
             -> l Window
             -> ModifiedLayout (DecorationEx TextDecoration DwmGeometry shrinker) l Window
dwmStyleDeco s theme showForFocused = decorationEx s theme TextDecoration (DwmGeometry showForFocused)

