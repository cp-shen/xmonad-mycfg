module Lib (entryPoint) where

import XMonad
import XMonad.Layout.ThreeColumns
import XMonad.Util.EZConfig
import XMonad.Util.Ungrab

entryPoint :: IO ()
entryPoint = xmonad $ def
  {
    modMask = mod4Mask
  , layoutHook = myLayouts
  }
  `additionalKeysP`
  [
    ("M-w", spawn "firefox")
  , ("M-t", spawn "kitty")
  ]

myLayouts = tiled ||| Mirror tiled ||| Full ||| threeCol
  where
    threeCol = ThreeCol nmaster delta ratio
    tiled = Tall nmaster delta ratio
    nmaster = 1
    ratio = 1/2
    delta = 3/100
