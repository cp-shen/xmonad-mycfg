module XMonad.MyCfg.MouseBindings (myMouseBindings) where

import Data.Map as M
import XMonad
import XMonad.Actions.CycleWS
import XMonad.StackSet as W

myMouseBindings :: XConfig Layout -> M.Map (KeyMask, Button) (Window -> X ())
myMouseBindings (XConfig {XMonad.modMask = modMask}) =
  M.fromList
    [ ((controlMask .|. shiftMask, button4), \w -> windows W.focusUp),
      ((controlMask .|. shiftMask, button5), \w -> windows W.focusDown),
      ((controlMask .|. shiftMask .|. mod1Mask, button4), \w -> moveTo Prev (Not emptyWS)),
      ((controlMask .|. shiftMask .|. mod1Mask, button5), \w -> moveTo Next (Not emptyWS))
    ]
