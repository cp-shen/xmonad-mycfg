--

module XMonad.MyCfg.Layouts (myLayouts) where

import XMonad
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed

myLayouts =
  -- onWorkspace wsWebpages (myTall ||| myFull) $
  -- onWorkspace wsTerminal (myGrid ||| myFull) $
  myTall ||| mySplit ||| myFull
  where
    myFull = renamed [Replace "Fu"] $ noBorders Full
    myTall = renamed [Replace "Ta"] $ smartBorders $ Tall 1 (5/100) (1/2)
    mySplit = renamed [Replace "Sp"] $ smartBorders $ Mirror $ Tall 1 (5/100) (6/10)
