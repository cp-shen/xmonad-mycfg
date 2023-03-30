--
module XMonad.MyCfg.ManageHooks where

import Control.Monad (liftM2)
import XMonad
import XMonad.Hooks.ManageHelpers
import XMonad.MyCfg.Workspaces
import qualified XMonad.StackSet as W

myManageHook :: ManageHook
myManageHook =
  composeAll . concat $
    [ --floating
      [isDialog --> doFloat],
      [className =? c --> doFloat | c <- myFloats],
      --shifting
      [(className =? c1 <||> className =? c2) --> f ws | (f, ws, c1, c2) <- shiftToWs]
    ]
  where
    doShiftAndGo = doF . liftM2 (.) W.greedyView W.shift
    myFloats = ["Gimp", "gimp", "xmessage", "Xmessage"]
    shiftToWs =
      [ (doShiftAndGo, wsTerminal, "Alacritty", ""),
        (doShiftAndGo, wsTerminal, "Bottom", ""),
        (doShiftAndGo, wsTerminal, "Glances", ""),
        (doShiftAndGo, wsClock, "Peaclock", ""),
        (doShiftAndGo, wsWebpages, "Chromium-browser", ""),
        (doShiftAndGo, wsWebpages, "Google-chrome", ""),
        (doShiftAndGo, wsWebpages, "firefox", ""),
        (doShiftAndGo, wsCode, "Emacs", ""),
        (doShiftAndGo, wsCode, "Code", ""),
        (doShiftAndGo, vsVideo, "mpv", ""),
        (doShiftAndGo, wsDownloads, "qBittorrent", "")
      ]
