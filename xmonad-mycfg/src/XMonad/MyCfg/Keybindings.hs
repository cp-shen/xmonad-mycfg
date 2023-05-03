module XMonad.MyCfg.Keybindings (myKeys) where

import qualified Data.Map as M
import Data.Tree
import System.Exit
import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.TreeSelect (TSConfig (..), TSNode (..), treeselectAction)
import XMonad.Actions.WindowGo
import qualified XMonad.MyCfg.ColorSchemes.OneDark as CS
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig

myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf = M.union keyMap $ mkKeymap conf strKeyMap
  where
    keyMap =
      M.fromList $
        [ ((m .|. modMask conf, k), f i)
          | (i, k) <- zip wsList [xK_1 .. xK_9],
            (f, m) <- [(toggleOrView', 0), (windows . W.shift, shiftMask)]
        ]
          ++ [ ((modMask conf, xK_0), toggleOrView' $ last wsList),
               ((modMask conf .|. shiftMask, xK_0), windows $ W.shift $ last wsList)
             ]
      where
        wsList = workspaces conf
        toggleOrView' = toggleOrDoSkip [] W.view

    strKeyMap =
      [ -- kill a client
        ("M-S-q", kill),
        -- change layouts
        -- ("M-S-n", sendMessage NextLayout),
        ("M-S-f", sendMessage $ JumpToLayout "Fu"),
        ("M-S-v", sendMessage $ JumpToLayout "Ta"),
        ("M-S-s", sendMessage $ JumpToLayout "Sp"),
        ("M-S-,", sendMessage $ IncMasterN 1),
        ("M-S-.", sendMessage $ IncMasterN (-1)),
        ("M-S-/", setLayout $ XMonad.layoutHook conf),
        -- manipulate screens
        ("M-h", prevScreen),
        ("M-l", nextScreen),
        ("M-S-h", shiftPrevScreen >> prevScreen),
        ("M-S-l", shiftNextScreen >> nextScreen),
        ("M-S-w", swapNextScreen >> nextScreen),
        -- focus movement
        ("M-j", windows W.focusDown),
        ("M-k", windows W.focusUp),
        ("M-<Space>", windows W.focusMaster),
        -- swap clients
        ("M-S-j", windows W.swapDown),
        ("M-S-k", windows W.swapUp),
        ("M-S-<Space>", windows W.swapMaster),
        -- shrink and expand
        ("M--", sendMessage Shrink),
        ("M-=", sendMessage Expand),
        -- toggle floating mode for clients
        ("M-S-t", withFocused $ windows . W.sink),
        ("M-t", withFocused toggleFloat),
        -- quit and restart
        ( "M-S-C-r",
          spawn $
            terminal conf
              ++ " --hold -e sh -c 'xmonad --recompile && xmonad --restart && echo ok! '"
        ),
        ("M-S-C-e", io exitSuccess),
        -- switch wotkspaces
        ("M-p", toggleWS),
        -- switch window using rofi
        ("M-S-r", spawn "rofi -show windowcd"),
        ("M-r", spawn "rofi -show drun"),
        -- application shortcuts
        -- ("M-w", runOrRaiseNext "google-chrome-stable" (className =? "Google-chrome")),
        ("M-f", runOrRaiseNext "firefox" (className =? "firefox")),
        ("M-e", runOrRaiseNext "emacs" (className =? "Emacs")), -- FIXME: use emacsclient?
        -- ("M-v", runOrRaiseNext "code" (className =? "Code")),
        ("M-v", runOrRaiseNext "neovide" (className =? "neovide")),
        ("M-<Return>", runOrRaiseNext "alacritty" (className =? "Alacritty")),
        ("M-S-<Return>", spawn "alacritty"),
        ("M-c", raiseNextMaybe (spawn "alacritty --class Peaclock -e peaclock") (className =? "Peaclock")),
        ("M-m", raiseNextMaybe (spawn "alacritty --class Bottom -e btm") (className =? "Bottom")),
        -- volume control using pulsemixer
        ("<XF86AudioMute>", spawn "pulsemixer --toggle-mute"),
        ("<XF86AudioLowerVolume>", spawn "pulsemixer --change-volume -5"),
        ("<XF86AudioRaiseVolume>", spawn "pulsemixer --change-volume +5"),
        -- toogle status bar
        ("M-b", spawn togglePolybarCmd),
        -- take a screenshot
        ("M-S-<F1>", spawn "xfce4-screenshooter -r"),
        -- treeSelect menu
        ("M-S-m", treeselectAction myTreeConf myTreeMenu)
      ]
      where
        blackP = read $ "0xff" ++ tail CS.black
        whiteP = read $ "0xff" ++ tail CS.lowWhite
        magentaP = read $ "0xff" ++ tail CS.magenta
        myTreeConf =
          def
            { ts_background = 0xaa000000,
              ts_node = (blackP, whiteP),
              ts_nodealt = (blackP, whiteP),
              ts_highlight = (blackP, magentaP),
              ts_extra = 0xff000000
            }
        myTreeMenu =
          [ Node (TSNode "Suspend" "" (spawn "systemctl suspend")) [],
            Node (TSNode "Hibernate" "" (spawn "systemctl hibernate")) [],
            Node (TSNode "ScreenOff" "" (spawn "xset dpms force off")) [],
            Node (TSNode "PowerOff" "" (spawn "systemctl poweroff")) [],
            Node (TSNode "ExitX" "" (io exitSuccess)) []
            -- , Node (TSNode "A Sub Menu" "" (return ()))
            --     [
            --       Node (TSNode "Hello1" "" (spawn "xmessage hello1!")) []
            --     , Node (TSNode "Hello2" "" (spawn "xmessage hello2!")) []
            --     ]
          ]
        togglePolybarCmd = "polybar-msg cmd toggle"
        toggleXmobarCmd =
          "dbus-send"
            ++ " --session"
            ++ " --dest=org.Xmobar.Control"
            ++ " --type=method_call"
            ++ " --print-reply"
            ++ " /org/Xmobar/Control"
            ++ " org.Xmobar.Control.SendSignal"
            ++ " \"string:Toggle 0\""
        toggleFloat :: Window -> X ()
        toggleFloat w =
          windows
            ( \s ->
                if M.member w (W.floating s)
                  then W.sink w s
                  else W.float w (W.RationalRect 0.1 0.1 0.5 0.5) $ W.shiftMaster s
            )
