-- |
module XMonad.MyCfg (entryPoint) where

import qualified Data.Map as M
import XMonad
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.StatusBar
import qualified XMonad.MyCfg.ColorSchemes.OneDark as Cs
import XMonad.MyCfg.Keybindings
import XMonad.MyCfg.Layouts
import XMonad.MyCfg.ManageHooks
import XMonad.MyCfg.StatusBar
import XMonad.MyCfg.Workspaces
import qualified XMonad.StackSet as W

entryPoint :: IO ()
entryPoint =
  xmonad $ ewmh $ withEasySB myStatusBarEntry myToggleStructsKey myConfig

myConfig =
  def
    { modMask = mod4Mask,
      terminal = "alacritty",
      workspaces = if null myWsList then map show [1 .. 9 :: Int] else myWsList ,
      layoutHook = myLayouts,
      manageHook = myManageHook,
      normalBorderColor = Cs.lowWhite,
      focusedBorderColor = Cs.magenta,
      keys = myKeys
    }
