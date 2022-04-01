-- |
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
      [(className =? c1 <||> className =? c2) --> f ws | (c1, c2, f, ws) <- shiftToWs]
    ]
  where
    doShiftAndGo = doF . liftM2 (.) W.greedyView W.shift
    myFloats = ["Gimp", "gimp", "xmessage", "Xmessage"]
    shiftToWs =
      [ ("Alacritty", "alacritty", doShiftAndGo, wsTerminal),
        ("Chromium", "chromium",   doShiftAndGo, wsWebpages),
        ("Emacs", "emacs",         doShiftAndGo, wsEmacs),
        ("Firefox", "firefox",     doShiftAndGo, wsWebpages),
        ("Glances", "glances",     doShiftAndGo, wsSystem),
        ("Mpv", "mpv",             doShift, wsMusic),
        ("qbittorrent", "qBittorrent", doShiftAndGo, wsDownloads)
      ]
