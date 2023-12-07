{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeOperators #-}

module XMonad.Layout.DecorationEx.TextDecoration where 

import qualified Data.Map as M

import XMonad
import XMonad.Prelude
import XMonad.Layout.Decoration (ModifiedLayout, Shrinker (..))
import XMonad.Util.Font

import XMonad.Layout.DecorationEx.LayoutModifier
import XMonad.Layout.DecorationEx.Types
import XMonad.Layout.DecorationEx.DecorationStyleEx
import XMonad.Layout.DecorationEx.Widgets

data TextDecoration a = TextDecoration
  deriving (Show, Read)

instance ClickHandler (GenericTheme SimpleStyle) StandardWidget where
  onDecorationClick theme button = M.lookup button (exOnDecoClick theme)
  isDraggingEnabled theme button = button `elem` exDragWindowButtons theme

instance DecorationEngine TextDecoration Window where
  type Theme TextDecoration = GenericTheme SimpleStyle
  type Widget TextDecoration = StandardWidget
  type DecorationPaintingContext TextDecoration = XPaintingContext
  type DecorationStyleState TextDecoration = XMonadFont

  describeEngine _ = "TextDecoration"

  calcWidgetPlace = calcTextWidgetPlace

  paintWidget = paintTextWidget

  paintDecoration = defaultPaintDecoration

  initializeState engine geom theme = initXMF (themeFontName theme)
  releaseStateResources engine = releaseXMF

  placeWidgets = defaultPlaceWidgets

paintTextWidget :: (Widget engine ~ StandardWidget,
                    Style (ThemeW engine) ~ SimpleStyle,
                    DecorationPaintingContext engine ~ XPaintingContext,
                    DecorationStyleState engine ~ XMonadFont,
                    DecorationEngine engine Window)
                => engine Window
                -> DecorationPaintingContext engine
                -> WidgetPlace
                -> DrawData engine
                -> Widget engine
                -> X ()
paintTextWidget deco (dpy, pixmap, gc) place dd widget = do
    let style = ddStyle dd
        x = rect_x (wpRectangle place)
        y = wpTextYPosition place
    str <- widgetString dd widget
    printStringXMF dpy pixmap (ddStyleState dd) gc (sTextColor style) (sTextBgColor style) x y str

calcTextWidgetPlace :: (Widget engine ~ StandardWidget,
                        DecorationStyleState engine ~ XMonadFont,
                        DecorationEngine engine Window)
                    => engine Window
                    -> DrawData engine
                    -> Widget engine
                    -> X WidgetPlace
calcTextWidgetPlace deco dd widget = do
    str <- widgetString dd widget
    let w = rect_width (ddDecoRect dd)
        h = rect_height (ddDecoRect dd)
        font = ddStyleState dd
    withDisplay $ \dpy -> do
      width <- fi <$> textWidthXMF dpy (ddStyleState dd) str
      (a, d) <- textExtentsXMF font str
      let height = a + d
          y = fi $ (h - fi height) `div` 2
          y0 = y + fi a
          rect = Rectangle 0 y width (fi height)
      return $ WidgetPlace y0 rect

textDecoration :: (Shrinker shrinker) => shrinker -> ThemeEx StandardWidget -> l Window
             -> ModifiedLayout (DecorationEx TextDecoration DefaultGeometry shrinker) l Window
textDecoration shrinker theme = decorationEx shrinker theme TextDecoration DefaultGeometry

