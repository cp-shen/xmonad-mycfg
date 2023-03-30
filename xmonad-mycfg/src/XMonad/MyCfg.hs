module XMonad.MyCfg (entryPoint) where

import qualified Data.Map as M
import XMonad
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.StatusBar
import qualified XMonad.MyCfg.ColorSchemes.OneDark as Cs
import XMonad.MyCfg.Keybindings
import XMonad.MyCfg.Layouts
import XMonad.MyCfg.ManageHooks
import XMonad.MyCfg.MouseBindings
import XMonad.MyCfg.StatusBar
import XMonad.MyCfg.Workspaces

entryPoint :: IO ()
entryPoint =
  xmonad $ docks $ ewmh $ withSB myPolybarEntry2 $ withSB myPolybarEntry1 myConfig

myConfig =
  def
    { modMask = mod4Mask,
      terminal = "alacritty",
      workspaces = if null myWsList then map show [1 .. 9 :: Int] else myWsList,
      layoutHook = avoidStruts myLayouts,
      manageHook = myManageHook,
      borderWidth = 2,
      normalBorderColor = Cs.lowWhite,
      focusedBorderColor = Cs.magenta,
      focusFollowsMouse = False,
      clickJustFocuses = True,
      keys = myKeys,
      mouseBindings = \c -> M.union (XMonad.mouseBindings def c) (myMouseBindings c)
    }
