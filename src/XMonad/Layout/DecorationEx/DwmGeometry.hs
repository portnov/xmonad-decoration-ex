{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module XMonad.Layout.DecorationEx.DwmGeometry (
    DwmGeometry (..),
    dwmStyleDeco
  ) where 

import XMonad
import qualified XMonad.StackSet as W
import XMonad.Layout.LayoutModifier
import qualified XMonad.Layout.Decoration as D

import XMonad.Layout.DecorationEx.LayoutModifier
import XMonad.Layout.DecorationEx.Types
import XMonad.Layout.DecorationEx.Engines
import XMonad.Layout.DecorationEx.Widgets
import XMonad.Layout.DecorationEx.TextEngine

-- | Decoration geometry data type
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

-- | Add a decoration to window layout. Widgets are indicated with text fragments using TextDecoration;
-- decoration placement is similar to DWM.
dwmStyleDeco :: D.Shrinker shrinker    
             => shrinker               -- ^ Strings shrinker, for example @shrinkText@
             -> ThemeEx StandardWidget -- ^ Decoration theme (font, colors, widgets, etc)
             -> Bool                   -- ^ If True, show decoration for active window as well
             -> l Window               -- ^ Layout to be decorated
             -> ModifiedLayout (DecorationEx TextDecoration DwmGeometry shrinker) l Window
dwmStyleDeco s theme showForFocused = decorationEx s theme TextDecoration (DwmGeometry showForFocused)

