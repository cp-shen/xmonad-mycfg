-- |
module XMonad.MyCfg.Keybindings (myKeys) where

import qualified Data.Map as M
import Data.Tree
import System.Exit
import XMonad
import XMonad.Actions.CycleRecentWS
import XMonad.Actions.CycleWS
-- import XMonad.Actions.EasyMotion
-- import XMonad.Actions.GridSelect
import XMonad.Actions.TreeSelect (TSConfig (..), TSNode (..), treeselectAction)
import XMonad.Actions.WindowGo
import XMonad.Hooks.ManageDocks
import qualified XMonad.MyCfg.ColorSchemes.OneDark as CS
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
    -- ++
    -- [ ((modMask conf, xK_0), windows $ W.greedyView $ last wsList),
    --   ((modMask conf .|. shiftMask, xK_0), windows $ W.shift $ last wsList) ]
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
    , ("M-]", windows W.focusDown)
    , ("M-j", windows W.focusDown)
    , ("M-<Tab>", windows W.focusDown)
    , ("M-[", windows W.focusUp)
    , ("M-k", windows W.focusUp)
    , ("M-S-<Tab>", windows W.focusUp)
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
    , ("M-S-]",      moveTo Next (Not emptyWS))
    , ("M1-<Tab>", moveTo Next (Not emptyWS))
    , ("M-S-[",        moveTo Prev (Not emptyWS))
    , ("M1-S-<Tab>", moveTo Prev (Not emptyWS))
    , ("M-p", toggleRecentNonEmptyWS)

      -- switch window using rofi
    --, ("M-<Tab>", spawn "rofi -show window ")
    --, ("M-<Delete>", spawn "rofi -show window ")
    , ("M-r", spawn "rofi -show drun")

      -- launch applications
    , ("M-w", runOrRaiseNext  "google-chrome-stable" (className =? "Google-chrome"))
    , ("M-f", runOrRaiseNext  "firefox" (className =? "firefox"))
    , ("M-e", runOrRaiseNext "emacs" (className =? "Emacs"))  --FIXME: use emacsclient?
    , ("M-<Return>", runOrRaiseNext  "alacritty" (className =? "Alacritty"))
    , ("M-S-<Return>", spawn  "alacritty")
    -- , ("M-g", spawn $ terminal conf
    --     ++ " --class glances,Glances -e glances")

      -- volume control using pulsemixer
    , ("<XF86AudioMute>", spawn "pulsemixer --toggle-mute")
    , ("<XF86AudioLowerVolume>", spawn "pulsemixer --change-volume -5")
    , ("<XF86AudioRaiseVolume>", spawn "pulsemixer --change-volume +5")

      -- toogle status bar
    , ("M-b", spawn togglePolybarCmd)
    -- , ("M-b", sendMessage (ToggleStrut U) <+> spawn togglePolybarCmd)

      -- take a screenshot
    , ("M-S-<F1>", spawn "xfce4-screenshooter -r")

      -- gridSelect menu
    -- , ("M-m", goToSelected def)
    -- , ("M-S-m", spawnSelected def ["firefox", "chromium"])

      -- treeSelect menu
    , ("M-S-m", treeselectAction myTreeConf myTreeMenu)

      -- esay motion
    -- , ("M-m", selectWindow def >>= (`whenJust` windows . W.focusWindow))
    ]
    where
      blackP = read $ "0xff" ++ tail CS.black
      whiteP = read $ "0xff" ++ tail CS.lowWhite
      magentaP = read $ "0xff" ++ tail CS.magenta
      myTreeConf = def {
          ts_background   = 0xaa000000
        , ts_node         = (blackP, whiteP)
        , ts_nodealt      = (blackP, whiteP)
        , ts_highlight    = (blackP, magentaP)
        , ts_extra        = 0xff000000
        }
      myTreeMenu = [
                     Node (TSNode "Suspend" "" (spawn "systemctl suspend")) []
                   , Node (TSNode "ScreenOff" "" (spawn "xset dpms force off")) []
                   , Node (TSNode "PowerOff" "" (spawn "systemctl poweroff")) []
                   , Node (TSNode "ExitX" "" (io exitSuccess)) []
                   -- , Node (TSNode "A Sub Menu" "" (return ()))
                   --     [
                   --       Node (TSNode "Hello1" "" (spawn "xmessage hello1!")) []
                   --     , Node (TSNode "Hello2" "" (spawn "xmessage hello2!")) []
                   --     ]
                   ]
      togglePolybarCmd = "polybar-msg cmd toggle"
      toggleXmobarCmd = "dbus-send"
            ++ " --session"
            ++ " --dest=org.Xmobar.Control"
            ++ " --type=method_call"
            ++ " --print-reply"
            ++ " /org/Xmobar/Control"
            ++ " org.Xmobar.Control.SendSignal"
            ++ " \"string:Toggle 0\""
