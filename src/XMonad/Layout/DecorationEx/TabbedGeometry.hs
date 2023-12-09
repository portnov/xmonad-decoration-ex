{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module XMonad.Layout.DecorationEx.TabbedGeometry where 

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

newtype TabbedGeometry a = TabbedGeometry Direction2D
  deriving (Show, Read)

instance DecorationGeometry TabbedGeometry Window where

  describeGeometry (TabbedGeometry _) = "Tabbed"

  pureDecoration (TabbedGeometry lc) (wt,ht) screenRect stack wrs (w, r) =
          Just $ case lc of
                      U -> upperTab
                      D -> lowerTab
                      L -> leftTab
                      R -> rightTab
        where
          Rectangle x y wh hh = r
          ws = filter (`elem` map fst (filter ((==r) . snd) wrs)) (W.integrate stack)
          loc k h i = k + fi ((h * fi i) `div` max 1 (fi $ length ws))
          esize k h = fi $ maybe k (\i -> loc k h (i+1) - loc k h i) $ w `elemIndex` ws
          wid = esize x wh
          n k h = maybe k (loc k h) $ w `elemIndex` ws
          nx = n x wh
          upperTab = Rectangle nx  y wid (fi ht)
          lowerTab = Rectangle nx (y + fi (hh - ht)) wid (fi ht)
          fixHeightLoc i = y + fi ht * fi i
          fixHeightTab k = Rectangle k
                (maybe y fixHeightLoc
                 $ w `elemIndex` ws) (fi wt) (fi ht)
          rightTab = fixHeightTab (x + fi (wh - wt))
          leftTab = fixHeightTab x
          numWindows = length ws

  shrinkWindow (TabbedGeometry loc) (Rectangle _ _ dw dh) (Rectangle x y w h)
        = case loc of
            U -> Rectangle x (y + fi dh) w (h - dh)
            D -> Rectangle x y w (h - dh)
            L -> Rectangle (x + fi dw) y (w - dw) h
            R -> Rectangle x y (w - dw) h

textTabbed :: (Shrinker shrinker) => shrinker -> ThemeEx StandardWidget -> l Window
             -> ModifiedLayout (DecorationEx TextDecoration TabbedGeometry shrinker) l Window
textTabbed shrinker theme = decorationEx shrinker theme TextDecoration (TabbedGeometry U)

