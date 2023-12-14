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

import XMonad.Layout.DecorationEx.LayoutModifier
import XMonad.Layout.DecorationEx.Types
import XMonad.Layout.DecorationEx.Engines
import XMonad.Layout.DecorationEx.Widgets
import XMonad.Layout.DecorationEx.TextEngine

data HorizontalTabPlacement = Top | Bottom
  deriving (Eq, Read, Show)

data VerticalTabPlacement = TabsAtLeft | TabsAtRight
  deriving (Eq, Read, Show)

data HorizontalTabWidth = AutoWidth | FixedWidth !Dimension
  deriving (Eq, Read, Show)

data HorizontalTabsAlignment = AlignTabsLeft | AlignTabsCenter | AlignTabsRight
  deriving (Eq, Read, Show)

data SingleTabMode = ShowTab | HideTab
  deriving (Eq, Read, Show)

data TabbedGeometry a =
      HorizontalTabs {
          showIfSingleWindow :: !SingleTabMode
        , hTabPlacement :: !HorizontalTabPlacement
        , hTabAlignment :: !HorizontalTabsAlignment
        , hTabWidth :: !HorizontalTabWidth
        , hTabHeight :: !Dimension
      }
    | VerticalTabs {
          showIfSingleWindow :: !SingleTabMode
        , vTabPlacement :: !VerticalTabPlacement
        , vTabWidth :: !Dimension
        , vTabHeight :: !Dimension
      }
  deriving (Show, Read)

instance Default (TabbedGeometry a) where
  def = HorizontalTabs ShowTab Top AlignTabsLeft AutoWidth 20 

instance DecorationGeometry TabbedGeometry Window where

  describeGeometry _ = "Tabbed"

  pureDecoration tabs screenRect stack wrs (window, windowRect) =
    let Rectangle windowX windowY windowWidth windowHeight = windowRect
        -- windows that are mapped onto the same rectangle as current one are considered to
        -- be in one tabs group
        tabbedWindows = filter (`elem` map fst (filter ((==windowRect) . snd) wrs)) (W.integrate stack)
        mbWindowIndex = window `elemIndex` tabbedWindows
        numWindows = length tabbedWindows
    in  if numWindows > 1 || (showIfSingleWindow tabs == ShowTab && numWindows > 0)
          then
            case tabs of
              HorizontalTabs {..} ->
                  Just $ case hTabPlacement of
                            Top    -> Rectangle decoX windowY effectiveTabWidth hTabHeight
                            Bottom -> Rectangle decoX (windowY + fi (windowHeight - hTabHeight)) effectiveTabWidth hTabHeight
                where
                  decoX = maybe windowX tabX mbWindowIndex

                  -- If there are too many windows or configured tab width
                  -- is too big, then we have to switch to 'auto' mode.
                  hTabWidth' =
                    case hTabWidth of
                      AutoWidth -> AutoWidth
                      FixedWidth tabWidth
                        | tabWidth * fi numWindows > windowWidth -> AutoWidth
                        | otherwise -> FixedWidth tabWidth

                  effectiveTabWidth =
                    case hTabWidth' of
                      AutoWidth -> fi $ maybe windowX (\i -> tabX (i+1) - tabX i) mbWindowIndex
                      FixedWidth tabWidth -> tabWidth

                  allTabsWidth =
                    case hTabWidth' of
                      AutoWidth -> fi windowWidth
                      FixedWidth tabWidth -> fi $ min windowWidth $ effectiveTabWidth * max 1 (fi numWindows)

                  tabsStartX =
                    case hTabAlignment of
                      AlignTabsLeft -> windowX
                      AlignTabsRight -> windowX + fi windowWidth - allTabsWidth
                      AlignTabsCenter -> windowX + (fi windowWidth - allTabsWidth) `div` 2

                  -- X coordinate of i'th window in horizontal tabs layout
                  tabX i = tabsStartX +
                        case hTabWidth' of
                          AutoWidth -> fi ((windowWidth * fi i) `div` max 1 (fi numWindows))
                          FixedWidth tabWidth -> fi effectiveTabWidth * fi i

              VerticalTabs {..} ->
                  Just $ case vTabPlacement of
                            TabsAtLeft  -> fixHeightTab windowX
                            TabsAtRight -> fixHeightTab (windowX + fi (windowWidth - vTabWidth))
                where
                  fixHeightLoc i = windowY + fi vTabHeight * fi i
                  fixHeightTab x = Rectangle x
                        (maybe windowY fixHeightLoc mbWindowIndex) vTabWidth vTabHeight
          else Nothing

  shrinkWindow tabs (Rectangle _ _ dw dh) (Rectangle x y w h) =
    case tabs of
      HorizontalTabs {..} ->
        case hTabPlacement of
            Top -> Rectangle x (y + fi dh) w (h - dh)
            Bottom -> Rectangle x y w (h - dh)
      VerticalTabs {..} ->
        case vTabPlacement of
            TabsAtLeft  -> Rectangle (x + fi dw) y (w - dw) h
            TabsAtRight -> Rectangle x y (w - dw) h

textTabbed :: (Shrinker shrinker) => shrinker -> ThemeEx StandardWidget -> l Window
             -> ModifiedLayout (DecorationEx TextDecoration StandardWidget TabbedGeometry shrinker) l Window
textTabbed shrinker theme = decorationEx shrinker theme TextDecoration def

