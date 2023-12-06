{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module XMonad.Layout.DecorationEx.TabbedTextDecoration where 

import XMonad
import qualified XMonad.StackSet as W
import XMonad.Prelude
import XMonad.Layout.Decoration (ModifiedLayout, Shrinker (..))
import XMonad.Util.Types

import XMonad.Layout.DecorationEx.LayoutModifier
import XMonad.Layout.DecorationEx.Types
import XMonad.Layout.DecorationEx.DecorationStyleEx
import XMonad.Layout.DecorationEx.Widgets
import XMonad.Layout.DecorationEx.TextDecoration

newtype TabbedTextDecoration a = TabbedTextDecoration Direction2D
  deriving (Show, Read)

instance DecorationStyleEx TabbedTextDecoration Window where
  type Theme TabbedTextDecoration = GenericTheme SimpleStyle
  type Widget TabbedTextDecoration = StandardWidget
  type DecorationPaintingContext TabbedTextDecoration = XPaintingContext

  describeDecoration (TabbedTextDecoration _) = "TabbedText"

  calcWidgetPlace = calcTextWidgetPlace

  paintWidget = paintTextWidget

  placeWidgets = defaultPlaceWidgets

  paintDecoration = defaultPaintDecoration

  pureDecoration (TabbedTextDecoration lc) theme screenRect stack wrs (w, r) =
          Just $ case lc of
                      U -> upperTab
                      D -> lowerTab
                      L -> leftTab
                      R -> rightTab
        where
          (wt, ht) = decorationSize theme
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

  shrinkWindow (TabbedTextDecoration loc) (Rectangle _ _ dw dh) (Rectangle x y w h)
        = case loc of
            U -> Rectangle x (y + fi dh) w (h - dh)
            D -> Rectangle x y w (h - dh)
            L -> Rectangle (x + fi dw) y (w - dw) h
            R -> Rectangle x y (w - dw) h

textTabbed :: (Shrinker s) => s -> ThemeEx StandardWidget -> l Window
             -> ModifiedLayout (DecorationEx TabbedTextDecoration s) l Window
textTabbed s theme = decorationEx s theme $ TabbedTextDecoration U

