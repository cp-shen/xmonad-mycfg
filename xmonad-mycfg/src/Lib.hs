module Lib (entryPoint) where

import XMonad
import XMonad.Layout.ThreeColumns
import XMonad.Util.EZConfig
import XMonad.Util.Ungrab
import qualified XMonad.StackSet as W
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Actions.CycleWS
import XMonad.Actions.CycleRecentWS
import XMonad.Hooks.EwmhDesktops


entryPoint :: IO ()
entryPoint = xmonad $ ewmhFullscreen $ ewmh $ myConfig

myConfig = def
  {
    modMask = mod4Mask
  , layoutHook = myLayouts
  , terminal = "kitty"
  }
  `additionalKeysP`
  [
    -- focus movement
    ("M-h", windows W.focusUp)
  , ("M-j", windows W.focusUp)
  , ("M-k", windows W.focusDown)
  , ("M-l", windows W.focusDown)

  -- swap clients
  , ("M-S-j", windows W.swapUp)
  , ("M-S-k", windows W.swapDown)

  -- shring and expand
  , ("M-S-h", sendMessage Shrink)
  , ("M-S-l", sendMessage Expand)

  -- switch wotkspaces
  , ("M-[", prevWS)
  , ("M-]", nextWS)
  , ("M-p", toggleRecentNonEmptyWS)

  -- launch applications
  , ("M-w", spawn "firefox")
  , ("M-t", spawn "kitty")
  , ("M-e", spawn "emacsclient -c")

  -- other misc key bindings
  , ("M-S-s", shellPrompt def)
  , ("M-S-r", spawn "kitty --hold sh -c 'xmonad --recompile && xmonad --restart'")
  ]

myLayouts = tiled ||| Mirror tiled ||| Full ||| threeCol
  where
    threeCol = ThreeCol nmaster delta ratio
    tiled = Tall nmaster delta ratio
    nmaster = 1
    ratio = 1/2
    delta = 3/100
