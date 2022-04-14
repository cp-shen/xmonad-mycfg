-- |

module XMonad.MyCfg.StatusBar (myStatusBarEntry) where

import XMonad
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Util.Loggers
import qualified XMonad.MyCfg.ColorSchemes.OneDark as Cs

-- myStatusBarEntry = statusBarProp "xmobar ~/.xmonad/xmobarrc" (pure myXmobarPP)
myStatusBarEntry = statusBarGeneric "polybar mybar" mempty

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
  , ppLayout          = lowWhite

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
