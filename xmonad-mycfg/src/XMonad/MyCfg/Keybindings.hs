-- |
module XMonad.MyCfg.Keybindings (myKeys) where

import qualified Data.Map as M
import System.Exit
import XMonad
import XMonad.Actions.CycleRecentWS
import XMonad.Actions.CycleWS
import XMonad.Actions.WindowGo
import XMonad.Hooks.ManageDocks
import XMonad.MyCfg.Workspaces
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig
import XMonad.Util.Types

myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf = M.union keyMap $ mkKeymap conf strKeyMap where

  keyMap = M.fromList $
    [ ((m .|. modMask conf, k), windows $ f i)
    | (i, k) <- zip wsList [xK_1 .. xK_9],
      (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)] ]
    ++
    [ ((modMask conf, xK_0), windows $ W.greedyView $ last wsList),
      ((modMask conf .|. shiftMask, xK_0), windows $ W.shift $ last wsList) ]
    where wsList = workspaces conf

  strKeyMap =
    [
      -- kill a client
      ("M-S-q", kill)

      -- change layouts
    , ("M-S-n", sendMessage NextLayout)
    , ("M-S-f", sendMessage $ JumpToLayout "Fu")
    , ("M-S-v", sendMessage $ JumpToLayout "Ta")
    , ("M-S-s", sendMessage $ JumpToLayout "Sp")
    , ("M-S-a", sendMessage $ JumpToLayout "Ac")
    , ("M-S-g", sendMessage $ JumpToLayout "Gr")

      -- focus movement
    , ("M-j", windows W.focusDown)
    , ("M-k", windows W.focusUp)
    , ("M-<Space>", windows W.focusMaster)

      -- swap clients
    , ("M-S-j", windows W.swapDown)
    , ("M-S-k", windows W.swapUp)
    , ("M-S-<Space>", windows W.swapMaster)

      -- shrink and expand
    , ("M-S-h", sendMessage Shrink)
    , ("M-S-l", sendMessage Expand)

      -- toggle floating mode for clients
    , ("M-t", withFocused $ windows . W.sink)

      -- quit and restart
    , ("M-S-C-r", spawn $ terminal conf
        ++ " --hold -e sh -c 'xmonad --recompile && xmonad --restart && echo ok! '")
    , ("M-S-C-e", io exitSuccess )

      -- switch wotkspaces
    , ("M-]",      moveTo Next (Not emptyWS))
    , ("M1-<Tab>", moveTo Next (Not emptyWS))
    , ("M-[",        moveTo Prev (Not emptyWS))
    , ("M1-S-<Tab>", moveTo Prev (Not emptyWS))
    , ("M-p", toggleRecentNonEmptyWS)

    -- switch window using rofi
    , ("M-<Tab>", spawn "rofi -show window ")
    , ("M-<Delete>", spawn "rofi -show window ")
    , ("M-r", spawn "rofi -show drun")

      -- launch applications
    , ("M-w", runOrRaiseNext  "google-chrome-stable" (className =? "Google-chrome"))
    , ("M-S-w", runOrRaiseNext  "firefox" (className =? "firefox"))
    , ("M-e", runOrRaiseNext "emacs" (className =? "Emacs"))  --FIXME: use emacsclient?
    , ("M-<Return>", runOrRaiseNext  "alacritty" (className =? "Alacritty"))
    -- , ("M-g", spawn $ terminal conf
    --     ++ " --class glances,Glances -e glances")

      -- volume control using pulsemixer
    , ("<XF86AudioMute>", spawn "pulsemixer --toggle-mute")
    , ("<XF86AudioLowerVolume>", spawn "pulsemixer --change-volume -5")
    , ("<XF86AudioRaiseVolume>", spawn "pulsemixer --change-volume +5")

      -- toogle status bar
    , ("M-b", spawn togglePolybarCmd)
    --, ("M-b", sendMessage (ToggleStrut U) <+> spawn togglePolybarCmd)

      -- take a screenshot
    , ("M-S-<F1>", spawn "xfce4-screenshooter -r")
    ]
    where
      togglePolybarCmd = "polybar-msg cmd toggle"
      toggleXmobarCmd = "dbus-send"
            ++ " --session"
            ++ " --dest=org.Xmobar.Control"
            ++ " --type=method_call"
            ++ " --print-reply"
            ++ " /org/Xmobar/Control"
            ++ " org.Xmobar.Control.SendSignal"
            ++ " \"string:Toggle 0\""
