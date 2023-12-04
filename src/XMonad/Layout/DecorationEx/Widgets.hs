{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module XMonad.Layout.DecorationEx.Widgets where 

import XMonad
import qualified XMonad.StackSet as W
import XMonad.Actions.DwmPromote
import qualified XMonad.Actions.CopyWindow as CW
import XMonad.Layout.Maximize
import XMonad.Actions.Minimize

import XMonad.Layout.DecorationEx.Types
import XMonad.Layout.DecorationEx.DecorationStyleEx

data StandardCommand =
      Noop
    | DwmPromote
    | ToggleSticky
    | ToggleMaximize
    | Minimize
    | CloseWindow
  deriving (Eq, Show, Read)

instance WindowCommand StandardCommand where
  executeWindowCommand Noop w = focus w
  executeWindowCommand CloseWindow w = killWindow w
  executeWindowCommand DwmPromote w = do
    focus w
    dwmpromote
  executeWindowCommand ToggleSticky w = do
    focus w
    copies <- CW.wsContainingCopies
    if null copies
      then windows CW.copyToAll
      else CW.killAllOtherCopies
  executeWindowCommand ToggleMaximize w = do
    sendMessage $ maximizeRestore w
    focus w
  executeWindowCommand Minimize w =
    minimizeWindow w

  isCommandChecked Noop _ = return False
  isCommandChecked CloseWindow _ = return False
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
  isCommandChecked ToggleMaximize _ = return False
  isCommandChecked Minimize _ = return False

data StandardWidget =
      TitleWidget
    | StandardWidget {
      swCheckedText :: String,
      swUncheckedText :: String,
      swCommand :: StandardCommand
    }
    deriving (Show, Read)


instance DecorationWidget StandardWidget where
  type WidgetCommand StandardWidget = StandardCommand

  widgetCommand TitleWidget = Noop
  widgetCommand w = swCommand w

isWidgetChecked :: DecorationWidget widget => widget -> Window -> X Bool
isWidgetChecked wdt = isCommandChecked (widgetCommand wdt)

widgetString :: (Widget dstyle ~ StandardWidget) => DrawData dstyle -> Widget dstyle -> X String
widgetString dd TitleWidget = return $ ddWindowTitle dd
widgetString dd w = do
  checked <- isWidgetChecked w (ddOrigWindow dd)
  if checked
    then return $ swCheckedText w
    else return $ swUncheckedText w

titleW = TitleWidget
toggleStickyW = StandardWidget "[S]" "[s]" ToggleSticky
minimizeW = StandardWidget "" "[_]" Minimize
maximizeW = StandardWidget "" "[O]" ToggleMaximize
closeW = StandardWidget "" "[X]" CloseWindow
dwmpromoteW = StandardWidget "[M]" "[m]" DwmPromote

