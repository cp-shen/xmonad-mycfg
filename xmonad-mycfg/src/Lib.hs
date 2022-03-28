module Lib
    ( someFunc
    ) where

import XMonad
import XMonad.Util.EZConfig
import XMonad.Util.Ungrab

someFunc :: IO ()
someFunc = xmonad def
