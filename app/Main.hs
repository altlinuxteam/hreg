-----------------------------------------------------------------------------
--
-- Module      :  Main
-- Copyright   :
-- License     :  BSD-3-Clause
--
-- Maintainer  :  omgbebebe@gmail.com
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Main where

import System.IO (hSetEncoding, stdout, utf8)
import HReg.CLI (hregCli)

main :: IO ()
main = hSetEncoding stdout utf8 >> hregCli
