module Lib (entryPoint) where

import XMonad
import XMonad.Layout.ThreeColumns
import XMonad.Util.EZConfig
import XMonad.Util.Ungrab
import qualified XMonad.StackSet as W

import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Actions.CycleWS


entryPoint :: IO ()
entryPoint = xmonad $ def
  {
    modMask = mod4Mask
  , layoutHook = myLayouts
  , terminal = "kitty"
  }
  `additionalKeysP`
  [
    ("M-w", spawn "firefox")
  , ("M-t", spawn "kitty")
  , ("M-e", spawn "emacsclient -c")
  , ("M-S-s", shellPrompt def) -- %! Move focus to the previous window
  , ("M-[", prevWS) -- %! Move focus to the previous window
  , ("M-]", nextWS) -- %! Move focus to the previous window
  ]

myLayouts = tiled ||| Mirror tiled ||| Full ||| threeCol
  where
    threeCol = ThreeCol nmaster delta ratio
    tiled = Tall nmaster delta ratio
    nmaster = 1
    ratio = 1/2
    delta = 3/100
