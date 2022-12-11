--

module XMonad.MyCfg.Layouts (myLayouts) where

import XMonad
import XMonad.Layout.BinarySpacePartition (emptyBSP)
import XMonad.Layout.Grid
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Renamed

myLayouts =
  -- onWorkspace wsWebpages (myTall ||| myFull) $
  -- onWorkspace wsTerminal (myGrid ||| myFull) $
  myGrid ||| myTall ||| myWide ||| myBinary ||| myFull
  where
    myFull = renamed [Replace "Fu"] $ noBorders Full
    myBinary = renamed [Replace "Bi"] $ smartBorders emptyBSP
    myTall = renamed [Replace "Ta"] $ smartBorders $ Tall nmaster delta ratio
    myWide = renamed [Replace "Wi"] $ smartBorders $ Mirror $ Tall nmaster delta (ratio + 0.1)
    myGrid = renamed [Replace "Gr"] $ smartBorders Grid
    nmaster = 1
    ratio = 1 / 2
    delta = 5 / 100
