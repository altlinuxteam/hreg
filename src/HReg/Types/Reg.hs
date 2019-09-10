-----------------------------------------------------------------------------
--
-- Module      :  HReg.Types
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

module HReg.Types.Reg (
    Registry(..),
    Action(..),
    Key(..),
    Value(..)
) where


type Key = Text

data Value
    = REG_NONE                       !ByteString
    | REG_SZ                         !ByteString
    | REG_EXPAND_SZ                  !ByteString
    | REG_BINARY                     !ByteString
    | REG_DWORD                      !Word32
    | REG_DWORD_LITTLE_ENDIAN        !Word32
    | REG_DWORD_BIG_ENDIAN           !Word32
    | REG_LINK                       !Text
    | REG_MULTI_SZ                   !(NonEmpty ByteString)
    | REG_RESOURCE_LIST              !(NonEmpty ByteString)
    | REG_FULL_RESOURCE_DESCRIPTION  !(NonEmpty ByteString)
    | REG_RESOURCE_REQUIREMENTS_LIST !(NonEmpty ByteString)
    | REG_QWORD                      !Word64
    | REG_QWORD_LITTLE_ENDIAN        !Word64
    deriving (Eq, Show)

data Registry = Registry
data Action
    = SetValue  !Key !Value -- set value at key
    | DelValues !Key !(NonEmpty Key) -- delete subkeys in key
    | DelValue !Key !Key -- delete subkey in key (is it a special case of `DelValues`?)
    | DelSubKeys !Key -- delete key and all subkeys
    | DeleteKeys !Key !(NonEmpty Key) -- delete subkeys within key
    | SecureKey !Key Bool
    deriving (Eq, Show)


