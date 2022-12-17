module XMonad.MyCfg.MouseBindings (myMouseBindings) where

import Data.Map as M
import XMonad
import XMonad.StackSet as W

myMouseBindings :: XConfig Layout -> M.Map (KeyMask, Button) (Window -> X ())
myMouseBindings (XConfig {XMonad.modMask = modMask}) =
  M.fromList
    [ ((modMask, button4), \w -> windows W.focusUp),
      ((modMask, button5), \w -> windows W.focusDown)
    ]
