--

module XMonad.MyCfg.Layouts (myLayouts) where

import XMonad
import XMonad.Layout.Accordion
import XMonad.Layout.Grid
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Renamed
import XMonad.MyCfg.Workspaces

myLayouts =
  -- onWorkspace wsWebpages (myTall ||| myFull) $
  -- onWorkspace wsEmacs myFull $
  onWorkspace wsTerminal (myGrid ||| myFull) $
  myTall ||| mySplit ||| myGrid ||| myFull ||| myAccord
  where
    myFull = renamed [Replace "Fu"] $ noBorders Full
    myAccord = renamed [Replace "Ac"] $ smartBorders Accordion
    myTall = renamed [Replace "Ta"] $ smartBorders $ Tall nmaster delta ratio
    mySplit = renamed [Replace "Sp"] $ smartBorders $ Mirror $ Tall nmaster delta (ratio + 0.1)
    myGrid = renamed [Replace "Gr"] $ smartBorders $ Grid
    nmaster = 1
    ratio = 1 / 2
    delta = 5 / 100
