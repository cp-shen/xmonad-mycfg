-- |

module XMonad.MyCfg.Layouts (myLayouts) where

import XMonad
import XMonad.Layout.NoBorders
import XMonad.Layout.ThreeColumns
import XMonad.Layout.Renamed

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
