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

instance DecorationStyleEx TextDecoration Window where
  type Theme TextDecoration = GenericTheme SimpleStyle
  type Widget TextDecoration = StandardWidget
  type DecorationPaintingContext TextDecoration = XPaintingContext
  type DecorationStyleState TextDecoration = XMonadFont

  describeDecoration _ = "TextDecoration"

  calcWidgetPlace = calcTextWidgetPlace

  paintWidget = paintTextWidget

  paintDecoration = defaultPaintDecoration

  initializeState dstyle theme = initXMF (themeFontName theme)
  releaseStateResources dstyle = releaseXMF

  placeWidgets = defaultPlaceWidgets

paintTextWidget :: (Widget dstyle ~ StandardWidget,
                    Style (ThemeW dstyle) ~ SimpleStyle,
                    DecorationPaintingContext dstyle ~ XPaintingContext,
                    DecorationStyleState dstyle ~ XMonadFont,
                    DecorationStyleEx dstyle Window)
                => dstyle Window
                -> DecorationPaintingContext dstyle
                -> WidgetPlace
                -> DrawData dstyle
                -> Widget dstyle
                -> X ()
paintTextWidget deco (dpy, pixmap, gc) place dd widget = do
    let style = ddStyle dd
        x = rect_x (wpRectangle place)
        y = wpTextYPosition place
    str <- widgetString dd widget
    printStringXMF dpy pixmap (ddStyleState dd) gc (sTextColor style) (sTextBgColor style) x y str

calcTextWidgetPlace :: (Widget dstyle ~ StandardWidget,
                        DecorationStyleState dstyle ~ XMonadFont,
                        DecorationStyleEx dstyle Window)
                    => dstyle Window
                    -> DrawData dstyle
                    -> Widget dstyle
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
             -> ModifiedLayout (DecorationEx TextDecoration shrinker) l Window
textDecoration s theme = decorationEx s theme TextDecoration

