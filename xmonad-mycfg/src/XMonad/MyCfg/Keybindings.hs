-- |
module XMonad.MyCfg.Keybindings (myKeys, myToggleStructsKey) where

import qualified Data.Map as M
import System.Exit
import XMonad
import XMonad.Actions.CycleRecentWS
import XMonad.Actions.CycleWS
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig

myToggleStructsKey XConfig {modMask = m} = (shiftMask .|. m, xK_b)

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
      -- launch terminal
      ("M-S-<Return>", spawn $ terminal conf)

      -- kill a client
    , ("M-S-q", kill)

      -- change layouts
    , ("M-<Space>", sendMessage NextLayout)
    --, ("M-S-<Space>", setLayout $ layoutHook myLayouts)

      -- focus movement
    , ("M-j", windows W.focusDown)
    , ("M-k", windows W.focusUp)
    , ("M-m", windows W.focusMaster)

      -- swap clients
    , ("M-S-j", windows W.swapDown)
    , ("M-S-k", windows W.swapUp)
    , ("M-S-m", windows W.swapMaster)

      -- shrink and expand
    , ("M-S-h", sendMessage Shrink)
    , ("M-S-l", sendMessage Expand)

      -- toggle floating mode for clients
    , ("M-t", withFocused $ windows . W.sink)

      -- quit and restart
    , ("M-S-r", spawn $ terminal conf
        ++ " --hold -e sh -c 'xmonad --recompile && xmonad --restart && echo ok! '")
    , ("M-S-e", io exitSuccess )

      -- switch wotkspaces
    , ("M-[", moveTo Prev (Not emptyWS))
    , ("M-]", moveTo Next (Not emptyWS))
    , ("M-p", toggleRecentNonEmptyWS)

      -- launch applications
    , ("M-w", spawn "firefox")
    , ("M-e", spawn "emacsclient -c")

      -- volume control using pulsemixer
    , ("<XF86AudioMute>", spawn "pulsemixer --toggle-mute")
    , ("<XF86AudioLowerVolume>", spawn "pulsemixer --change-volume -5")
    , ("<XF86AudioRaiseVolume>", spawn "pulsemixer --change-volume +5")

      -- other misc key bindings
    -- , ("M-S-s", shellPrompt def)
    ]
