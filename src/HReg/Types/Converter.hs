-----------------------------------------------------------------------------
--
-- Module      :  HReg.Types.Converter
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

module HReg.Types.Converter (
    mkVal,
    encode,
    decode,
    encodeDataOnly,
    encodeToStore,
    utf8toUtf16,
    utf16toUtf8
    ) where

import qualified Data.ByteString as BS
import           Data.Serialize
import           Data.Text.Encoding (encodeUtf16LE, decodeUtf16LE)
import           HReg.Types.Reg
import           Prelude hiding (get)


encodeDataOnly :: Value -> ByteString
encodeDataOnly = BS.drop 8 . encode

encodeToStore :: Value -> ByteString
encodeToStore v@(REG_SZ val) = BS.init . encodeDataOnly $ v
encodeToStore v@(REG_EXPAND_SZ val) = BS.init . encodeDataOnly $ v
encodeToStore v@(REG_MULTI_SZ val) = BS.intercalate "\n" . BS.split 0 . BS.init . BS.init . encodeDataOnly $ v
encodeToStore v@(REG_LINK val) = encodeDataOnly $ v
encodeToStore v = encodeDataOnly v

utf8toUtf16 :: ByteString -> ByteString
utf8toUtf16 = encodeUtf16LE . decodeUtf8

utf16toUtf8 :: ByteString -> ByteString
utf16toUtf8 = encodeUtf8 . decodeUtf16LE

instance Serialize Value where
    put (REG_NONE                       v) = ty  0 >> regBinary v
    put (REG_SZ                         v) = ty  1 >> bsLen v >> putByteString v >> putInt32le 0
    put (REG_EXPAND_SZ                  v) = ty  2 >> bsLen v >> putByteString v >> putInt32le 0
    put (REG_BINARY                     v) = ty  3 >> regBinary v
    put (REG_DWORD                      v) = ty  4 >> len   4 >> putWord32le v
    put (REG_DWORD_LITTLE_ENDIAN        v) = ty  4 >> len   4 >> putWord32le v
    put (REG_DWORD_BIG_ENDIAN           v) = ty  5 >> len   4 >> putWord32be v
    put (REG_LINK                       v) = ty  6 >> bsLen v' >> putByteString v' where v' = encodeUtf16LE v
    put (REG_MULTI_SZ                   v) = ty  7 >> bsLen v' >> putByteString v' where v' = mkMultiList v
    put (REG_RESOURCE_LIST              v) = ty  8 >> regBinary v
    put (REG_FULL_RESOURCE_DESCRIPTION  v) = ty  9 >> regBinary v
    put (REG_RESOURCE_REQUIREMENTS_LIST v) = ty 10 >> regBinary v
    put (REG_QWORD                      v) = ty 11 >> len 8 >> putWord64le v
    put (REG_QWORD_LITTLE_ENDIAN        v) = ty 12 >> len 8 >> putWord64le v
    get = do
        ty <- getInt32le
        len <- getInt32le
        val <- replicateM (fromIntegral len) getWord8
        pure $! mkVal ty (BS.pack val)

mkMultiList :: NonEmpty ByteString -> ByteString
mkMultiList = flip BS.snoc 0 . flip BS.snoc 0 . BS.intercalate "\0" . toList

fromMultiList :: ByteString -> NonEmpty ByteString
fromMultiList = fromList . BS.split 0 . BS.init

ty = putInt32le
bsLen bs = putInt32le (fromIntegral (BS.length bs))
len = putInt32le
regBinary v = bsLen v >> putByteString v
err s t = error $ "cannot parse '" <> show s <> "' as a '" <> t <> "'"

mkVal :: Int32 -> ByteString -> Value
mkVal  0 v = REG_NONE v
mkVal  1 v = REG_SZ v
mkVal  2 v = REG_EXPAND_SZ v
mkVal  3 v = REG_BINARY v
mkVal  4 v = REG_DWORD <$> fromRight (err v "word32le") $ runGet getWord32le v
mkVal  5 v = REG_DWORD_BIG_ENDIAN <$> fromRight (err v "word32be") $ runGet getWord32be v
mkVal  6 v = REG_LINK $ decodeUtf16LE v
mkVal  7 v = REG_MULTI_SZ $ fromMultiList v
mkVal  8 v = REG_RESOURCE_LIST v
mkVal  9 v = REG_FULL_RESOURCE_DESCRIPTION v
mkVal 10 v = REG_RESOURCE_REQUIREMENTS_LIST v
mkVal 11 v = REG_QWORD <$> fromRight (err v "word64le") $ runGet getWord64le v
mkVal 12 v = REG_QWORD_LITTLE_ENDIAN <$> fromRight (err v "word64be") $ runGet getWord64be v
