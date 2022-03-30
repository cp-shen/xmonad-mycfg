-- |

module XMonad.MyCfg.ManageHooks where

import XMonad
import XMonad.Hooks.ManageHelpers
import XMonad.MyCfg.Workspaces
import Control.Monad (liftM2)
import qualified XMonad.StackSet as W


myManageHook :: ManageHook
myManageHook = composeAll . concat $
  [
    --floating
    [isDialog --> doFloat]
  , [className =? c --> doFloat | c <- myFloats]
    --shifting
  , [(className =? c1 <||> className =? c2) --> f ws | (c1, c2, f, ws) <- shiftToWs ]
  ]
  where
  doShiftAndGo = doF . liftM2 (.) W.greedyView W.shift
  myFloats = ["Gimp", "gimp"]
  shiftToWs =
    [
      ("Glances",  "glances",  doShiftAndGo, wsSystem)
    , ("Mpv",      "mpv",      doShiftAndGo, wsMusic)
    , ("Alacritty", "alacritty", doShiftAndGo, wsTerminal)
    , ("Firefox",  "firefox",  doShiftAndGo, wsWebpages)
    , ("Chromium", "chromium", doShiftAndGo, wsWebpages)
    , ("Emacs",    "emacs",    doShiftAndGo, wsEmacs)
    ]
