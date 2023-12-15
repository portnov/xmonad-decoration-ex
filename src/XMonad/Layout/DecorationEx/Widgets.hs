{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module XMonad.Layout.DecorationEx.Widgets where 

import XMonad
import qualified XMonad.StackSet as W
import XMonad.Actions.DwmPromote
import qualified XMonad.Actions.CopyWindow as CW
import qualified XMonad.Layout.Groups.Examples as Ex
import XMonad.Layout.Maximize
import XMonad.Actions.Minimize

import XMonad.Layout.DecorationEx.Types
import XMonad.Layout.DecorationEx.Engine

data StandardCommand =
      FocusWindow
    | FocusUp
    | FocusDown
    | MoveToNextGroup
    | MoveToPrevGroup
    | DwmPromote
    | ToggleSticky
    | ToggleMaximize
    | Minimize
    | CloseWindow
  deriving (Eq, Show, Read)

instance WindowCommand StandardCommand where
  executeWindowCommand FocusWindow w = do
    focus w
    return False
  executeWindowCommand FocusUp _ = do
    windows W.focusUp
    withFocused maximizeWindowAndFocus
    return True
  executeWindowCommand FocusDown _ = do
    windows W.focusDown
    withFocused maximizeWindowAndFocus
    return True
  executeWindowCommand MoveToNextGroup w = do
    focus w
    Ex.moveToGroupDown False
    return True
  executeWindowCommand MoveToPrevGroup w = do
    focus w
    Ex.moveToGroupUp False
    return True
  executeWindowCommand CloseWindow w = do
    killWindow w
    return True
  executeWindowCommand DwmPromote w = do
    focus w
    dwmpromote
    return True
  executeWindowCommand ToggleSticky w = do
    focus w
    copies <- CW.wsContainingCopies
    if null copies
      then windows CW.copyToAll
      else CW.killAllOtherCopies
    return True
  executeWindowCommand ToggleMaximize w = do
    sendMessage $ maximizeRestore w
    focus w
    return True
  executeWindowCommand Minimize w = do
    minimizeWindow w
    return True

  isCommandChecked FocusWindow _ = return False
  isCommandChecked DwmPromote w = do
      withWindowSet $ \ws -> return $ Just w == master ws
    where
      master ws =
        case W.integrate' $ W.stack $ W.workspace $ W.current ws of
          [] -> Nothing
          (x:_) -> Just x
  isCommandChecked ToggleSticky w = do
    ws <- gets windowset
    let copies = CW.copiesOfOn (Just w) (CW.taggedWindows $ W.hidden ws)
    return $ not $ null copies
  isCommandChecked _ _ = return False

data GenericWidget cmd =
      TitleWidget
    | GenericWidget {
      swCheckedText :: String,
      swUncheckedText :: String,
      swCommand :: cmd
    }
    deriving (Show, Read)

type StandardWidget = GenericWidget StandardCommand

instance DecorationWidget (GenericWidget StandardCommand) where

  type WidgetCommand (GenericWidget StandardCommand) = StandardCommand

  widgetCommand TitleWidget _ = FocusWindow
  widgetCommand w 1 = swCommand w
  widgetCommand _ _ = FocusWindow

  isShrinkable TitleWidget = True
  isShrinkable _ = False

isWidgetChecked :: DecorationWidget widget => widget -> Window -> X Bool
isWidgetChecked wdt = isCommandChecked (widgetCommand wdt 1)

class DecorationWidget widget => TextWidget widget where
  widgetString :: DrawData engine widget -> widget -> X String

instance TextWidget StandardWidget where
    widgetString dd TitleWidget = return $ ddWindowTitle dd
    widgetString dd w = do
      checked <- isWidgetChecked w (ddOrigWindow dd)
      if checked
        then return $ swCheckedText w
        else return $ swUncheckedText w

titleW = TitleWidget
toggleStickyW = GenericWidget "[S]" "[s]" ToggleSticky
minimizeW = GenericWidget "" "[_]" Minimize
maximizeW = GenericWidget "" "[O]" ToggleMaximize
closeW = GenericWidget "" "[X]" CloseWindow
dwmpromoteW = GenericWidget "[M]" "[m]" DwmPromote
moveToNextGroupW = GenericWidget "" "[>]" MoveToNextGroup
moveToPrevGroupW = GenericWidget "" "[<]" MoveToPrevGroup

