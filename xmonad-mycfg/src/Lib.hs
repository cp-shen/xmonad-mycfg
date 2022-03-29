module Lib (entryPoint) where

import XMonad
import qualified XMonad.StackSet as W
import XMonad.Actions.CycleWS
import XMonad.Actions.CycleRecentWS
import XMonad.Util.EZConfig
import XMonad.Layout.ThreeColumns
import System.Exit


entryPoint :: IO ()
entryPoint = xmonad myConfig

myConfig = def
  {
    modMask = mod4Mask
  , layoutHook = myLayouts
  , terminal = "kitty"
  }

  `additionalKeysP`
  [
    -- launch terminal
    ("M-S-<Return>", spawn $ terminal myConfig)

    -- kill a client
  , ("M-S-q", kill)

    -- change layouts
  , ("M-<Space>", sendMessage NextLayout)
  --, ("M-S-<Space>", setLayout myLayouts)

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
  , ("M-S-r", spawn "kitty --hold sh -c 'xmonad --recompile && xmonad --restart'")
  , ("M-S-e", io (exitWith ExitSuccess))

    -- switch wotkspaces
  , ("M-[", prevWS)
  , ("M-]", nextWS)
  , ("M-p", toggleRecentNonEmptyWS)

    -- launch applications
  , ("M-w", spawn "firefox")
  , ("M-e", spawn "emacsclient -c")


    -- other misc key bindings
  -- , ("M-S-s", shellPrompt def)
  ]

  -- mod-[1..9] %! Switch to workspace N
  -- mod-shift-[1..9] %! Move client to workspace N
  `additionalKeys`
    [((m .|. modMask myConfig, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces myConfig) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]

  -- mod-{w,e,r} %! Switch to physical/Xinerama screens 1, 2, or 3
  -- mod-shift-{w,e,r} %! Move client to screen 1, 2, or 3
  `additionalKeys`
    [((m .|. modMask myConfig, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

myLayouts = tiled ||| Mirror tiled ||| Full ||| threeCol
  where
    threeCol = ThreeCol nmaster delta ratio
    tiled = Tall nmaster delta ratio
    nmaster = 1
    ratio = 1/2
    delta = 3/100
