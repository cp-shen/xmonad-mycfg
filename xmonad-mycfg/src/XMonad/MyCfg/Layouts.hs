-- |

module XMonad.MyCfg.Layouts (myLayouts) where

import XMonad
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed
import XMonad.Layout.Accordion
import XMonad.Layout.PerWorkspace
import XMonad.MyCfg.Workspaces

myLayouts =
  onWorkspace wsTerminal (myAccord ||| myFull) $
  onWorkspace wsWebpages (myTall ||| myFull) $
  onWorkspace wsEmacs myFull $
  mySplit ||| myFull
  where
    myFull =   renamed [Replace "Fu"] $ noBorders Full
    myAccord = renamed [Replace "Ac"] $ smartBorders Accordion
    myTall =   renamed [Replace "Ta"] $ smartBorders $ Tall nmaster delta ratio
    mySplit =  renamed [Replace "Sp"] $ smartBorders $ Mirror $ Tall nmaster delta (ratio+0.2)
    nmaster = 1
    ratio = 1/2
    delta = 5/100
