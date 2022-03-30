-- |

module XMonad.MyCfg.ManageHooks where

import XMonad.Hooks.ManageHelpers
import XMonad

myManageHook :: ManageHook
myManageHook = composeAll
  [
    className =? "Gimp"    --> doFloat
  , className =? "Glances" --> doShift "sys"
  , isDialog               --> doFloat
  ]
