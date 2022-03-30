module Lib (entryPoint) where

import XMonad
import qualified XMonad.StackSet as W
import XMonad.Actions.CycleWS
import XMonad.Actions.CycleRecentWS
import XMonad.Util.EZConfig
import XMonad.Layout.ThreeColumns
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops
import System.Exit
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Hooks.DynamicLog
import XMonad.Layout.Magnifier
import XMonad.Util.Loggers
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.NoBorders
import qualified ColorSchemes.OneDark as Cs
import XMonad.Layout.Renamed

entryPoint :: IO ()
entryPoint = xmonad
-- $ ewmhFullscreen
  $ ewmh
  $ withEasySB (statusBarProp "xmobar" (pure myXmobarPP)) toggleStrutsKey
  $ myConfig
  where toggleStrutsKey XConfig { modMask = m } = (shiftMask .|. m , xK_b)

myManageHook :: ManageHook
myManageHook = composeAll
    [ className =? "Gimp" --> doFloat
    , isDialog            --> doFloat
    ]

myXmobarPP :: PP
myXmobarPP = def
    {
      ppSep             = magenta " • "
    , ppTitleSanitize   = xmobarStrip

    , ppCurrent         = white . wrap " " "" . xmobarBorder "Top" "#8be9fd" 2
    , ppVisible         = white . wrap " " ""
    , ppHidden          = white . wrap " " ""
    , ppHiddenNoWindows = lowWhite . wrap " " ""
    , ppUrgent          = red . wrap (yellow "!") (yellow "!")

    , ppOrder           = \(ws : l : cur_win : wins : _) -> [ws, l, wins]
    , ppExtras          = [logTitles formatFocused formatUnfocused]
    }
  where
    formatFocused   = wrap (white    "[") (white    "]") . magenta . ppWindow
    formatUnfocused = wrap (lowWhite "[") (lowWhite "]") . blue    . ppWindow

    -- | Windows should have *some* title, which should not not exceed a
    -- sane length.
    ppWindow :: String -> String
    ppWindow = xmobarRaw . (\w -> if null w then "untitled" else w) . shorten 30

    blue, lowWhite, magenta, red, white, yellow :: String -> String
    magenta  = xmobarColor Cs.magenta ""
    blue     = xmobarColor Cs.blue ""
    white    = xmobarColor Cs.white ""
    yellow   = xmobarColor Cs.yellow ""
    red      = xmobarColor Cs.red ""
    lowWhite = xmobarColor Cs.lowWhite ""


myConfig = def
  {
    modMask = mod4Mask
  , workspaces = ["term", "web", "sys", "mus", "dow", "ws6", "ws7", "ws8", "ws9", "emacs"]
  , terminal = "alacritty"
  , layoutHook = myLayouts
  , manageHook = myManageHook
  , normalBorderColor = Cs.lowWhite
  , focusedBorderColor = Cs.magenta
  }

  `additionalKeysP`
  [
    -- launch terminal
    ("M-S-<Return>", spawn $ terminal myConfig)

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
  , ("M-S-r", spawn $ terminal myConfig ++ " --hold -e sh -c 'xmonad --recompile && xmonad --restart && echo ok! '")
  , ("M-S-e", io (exitWith ExitSuccess))

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

  -- mod-[1..9] %! Switch to workspace N
  -- mod-shift-[1..9] %! Move client to workspace N
  `additionalKeys`
  [((m .|. modMask myConfig, k), windows $ f i)
      | (i, k) <- zip (XMonad.workspaces myConfig) [xK_1 .. xK_9]
      , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
  `additionalKeys`
  [
    ((modMask myConfig, xK_0),               windows $ W.greedyView "emacs")
  , ((modMask myConfig .|. shiftMask, xK_0), windows $ W.shift "emacs")
  ]

myLayouts =
      renamed [Replace "Ta"] myTall
  ||| renamed [Replace "Sp"] mySplit
  ||| renamed [Replace "3C"] myThreeCol
  ||| renamed [Replace "Fu"] myFull
  where
    myTall = smartBorders $ Tall nmaster delta ratio
    mySplit = smartBorders $ Mirror $ Tall nmaster delta (ratio+0.2)
    myThreeCol = smartBorders $ ThreeColMid nmaster delta ratio
    myFull = noBorders Full
    nmaster = 1
    ratio = 1/2
    delta = 5/100
