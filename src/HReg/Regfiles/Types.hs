-----------------------------------------------------------------------------
--
-- Module      :  HReg.Regfiles.Types
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

module HReg.Regfiles.Types (
    Path(..),
    KeyName(..)
) where



newtype Path = Path FilePath deriving (Eq, Show)
newtype KeyName = KeyName FilePath deriving (Eq, Show)
