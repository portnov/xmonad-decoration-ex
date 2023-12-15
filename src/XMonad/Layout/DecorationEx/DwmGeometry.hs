{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module XMonad.Layout.DecorationEx.DwmGeometry (
    DwmGeometry (..),
    dwmStyleDeco, dwmStyleDecoEx
  ) where 

import Data.Default

import XMonad
import qualified XMonad.StackSet as W
import XMonad.Layout.LayoutModifier
import qualified XMonad.Layout.Decoration as D

import XMonad.Layout.DecorationEx.LayoutModifier
import XMonad.Layout.DecorationEx.Types
import XMonad.Layout.DecorationEx.Geometry
import XMonad.Layout.DecorationEx.Widgets
import XMonad.Layout.DecorationEx.TextEngine

-- | Decoration geometry data type
data DwmGeometry a = DwmGeometry {
      dwmShowForFocused :: !Bool
    , dwmHorizontalPosition :: !Rational
    , dwmDecoHeight :: !Dimension
    , dwmDecoWidth :: !Dimension
  }
  deriving (Show, Read)

instance Default (DwmGeometry a) where
  def = DwmGeometry False 1 20 200

fi :: (Integral a, Num b) => a -> b
fi = fromIntegral

instance DecorationGeometry DwmGeometry Window where
  describeGeometry _ = "DwmStyle"

  pureDecoration (DwmGeometry {..}) screenRect stack wrs (w, Rectangle x y windowWidth windowHeight) =
    let width = min windowWidth dwmDecoWidth
        halfWidth = width `div` 2
        minCenterX = x + fi halfWidth
        maxCenterX = x + fi windowWidth - fromIntegral halfWidth
        centerX = round $ (1 - dwmHorizontalPosition)*fi minCenterX + dwmHorizontalPosition*fi maxCenterX
        decoX = centerX - fi halfWidth
        focusedWindow = W.focus stack
        isFocused = focusedWindow == w
    in  if (not dwmShowForFocused && isFocused) || not (D.isInStack stack w)
          then Nothing
          else Just $ Rectangle (fi decoX) y width dwmDecoHeight

  shrinkWindow _ decoRect windowRect = windowRect

-- | Add a decoration to window layout. Widgets are indicated with text fragments using TextDecoration;
-- decoration placement can be adjusted.
dwmStyleDecoEx :: D.Shrinker shrinker    
             => shrinker               -- ^ Strings shrinker, for example @shrinkText@
             -> DwmGeometry Window
             -> ThemeEx StandardWidget -- ^ Decoration theme (font, colors, widgets, etc)
             -> l Window               -- ^ Layout to be decorated
             -> ModifiedLayout (DecorationEx TextDecoration StandardWidget DwmGeometry shrinker) l Window
dwmStyleDecoEx shrinker geom theme = decorationEx shrinker theme TextDecoration geom

-- | Add a decoration to window layout. Widgets are indicated with text fragments using TextDecoration;
-- decoration placement is similar to DWM.
dwmStyleDeco :: D.Shrinker shrinker    
             => shrinker               -- ^ Strings shrinker, for example @shrinkText@
             -> ThemeEx StandardWidget -- ^ Decoration theme (font, colors, widgets, etc)
             -> l Window               -- ^ Layout to be decorated
             -> ModifiedLayout (DecorationEx TextDecoration StandardWidget DwmGeometry shrinker) l Window
dwmStyleDeco shrinker = dwmStyleDecoEx shrinker def

