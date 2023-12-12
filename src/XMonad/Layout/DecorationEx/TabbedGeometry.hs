{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module XMonad.Layout.DecorationEx.TabbedGeometry where 

import Data.Default

import XMonad
import qualified XMonad.StackSet as W
import XMonad.Prelude
import XMonad.Layout.Decoration (ModifiedLayout, Shrinker (..))
import XMonad.Util.Types

import XMonad.Layout.DecorationEx.LayoutModifier
import XMonad.Layout.DecorationEx.Types
import XMonad.Layout.DecorationEx.Engines
import XMonad.Layout.DecorationEx.Widgets
import XMonad.Layout.DecorationEx.TextEngine

data TabbedGeometry a = TabbedGeometry {
      tabPlacement :: !Direction2D
    , tabHeight :: !Dimension
    , tabWidth :: !Dimension
  }
  deriving (Show, Read)

instance Default (TabbedGeometry a) where
  def = TabbedGeometry U 20 200

instance DecorationGeometry TabbedGeometry Window where

  describeGeometry _ = "Tabbed"

  pureDecoration (TabbedGeometry {..}) screenRect stack wrs (window, windowRect) =
          Just $ case tabPlacement of
                      U -> Rectangle decoX  windowY effectiveTabWidth tabHeight
                      D -> Rectangle decoX (windowY + fi (windowHeight - tabHeight)) effectiveTabWidth tabHeight
                      L -> fixHeightTab windowX
                      R -> fixHeightTab (windowX + fi (windowWidth - tabWidth))
        where
          Rectangle windowX windowY windowWidth windowHeight = windowRect
          -- windows that are mapped onto the same rectangle as current one are considered to
          -- be in one tabs group
          tabbedWindows = filter (`elem` map fst (filter ((==windowRect) . snd) wrs)) (W.integrate stack)
          -- X coordinate of i'th window in horizontal tabs layout
          tabX i = windowX + fi ((windowWidth * fi i) `div` max 1 (fi $ length tabbedWindows))
          effectiveTabWidth = fi $ maybe windowX (\i -> tabX (i+1) - tabX i) $ window `elemIndex` tabbedWindows
          decoX = maybe windowX tabX $ window `elemIndex` tabbedWindows
          fixHeightLoc i = windowY + fi tabHeight * fi i
          fixHeightTab k = Rectangle k
                (maybe windowY fixHeightLoc
                 $ window `elemIndex` tabbedWindows) tabWidth tabHeight
          numWindows = length tabbedWindows

  shrinkWindow (TabbedGeometry {..}) (Rectangle _ _ dw dh) (Rectangle x y w h)
        = case tabPlacement of
            U -> Rectangle x (y + fi dh) w (h - dh)
            D -> Rectangle x y w (h - dh)
            L -> Rectangle (x + fi dw) y (w - dw) h
            R -> Rectangle x y (w - dw) h

textTabbed :: (Shrinker shrinker) => shrinker -> ThemeEx StandardWidget -> l Window
             -> ModifiedLayout (DecorationEx TextDecoration TabbedGeometry shrinker) l Window
textTabbed shrinker theme = decorationEx shrinker theme TextDecoration def

