-----------------------------------------------------------------------------
--
-- Module      :  Regfiles
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

module HReg.Regfiles (
--    parseBranch,
    exportReg
    --
--    toReg,
--    showPath,
--    showKey,
--    Path(..),
--    KeyName(..)
) where

import           Data.Binary.Get
import           Conduit
import qualified Data.Conduit.Combinators as C
import qualified Data.ByteString as BS
import           Data.ByteString.Builder
import qualified Data.ByteString.Char8 as BC
import           Data.ByteString.Encoding
import qualified Data.ByteString.Lazy as BL
import           HReg (readKey)
import           HReg.Types
import           System.FilePath
import           System.FilePath.Find
import           System.FilePath.Posix (makeRelative)
import           System.Directory
import           System.PosixCompat.Files
import           Prelude hiding (find)

newtype Path = Path FilePath deriving (Eq, Show)
newtype KeyName = KeyName FilePath deriving (Eq, Show)
--newtype KeyVal = KeyVal (Text, Value) deriving (Eq, Show)

parseBranch :: FilePath -> FilePath -> IO [(Path, [KeyName])]
parseBranch root path = do
    dirs <- find always (fileType ==? Directory) $ root </> path
    concat <$> mapM (\x -> do
        content <- listDirectory x
        fs <- filterM (\y -> isRegularFile <$> getFileStatus (x </> y)) content
        pure [(Path x, map KeyName fs)]
        ) (filter (/= root) dirs)

exportReg :: FilePath -> FilePath -> FilePath -> IO ()
exportReg root path output = do
    let header = "Windows Registry Editor Version 5.00"
    b <- parseBranch root path
    res <- mapM (\(Path p, ks) -> toReg root p ks) b
    BS.writeFile output $ BS.pack [0xff, 0xfe] <> utf8toUtf16 (header <> "\n\n" <> BS.concat res)
{-
    runConduit
         $ yieldMany b
        .| C.mapM (\(Path p, ks) -> toReg p ks)
        .| sinkFile output
-}

utf8toUtf16 :: ByteString -> ByteString
utf8toUtf16 = encode utf16le . decode utf8

toReg :: FilePath -> FilePath -> [KeyName] -> IO ByteString
toReg root p [] = pure $ showPath root p <> "\n\n"
toReg root p ks = do
    let path = showPath root p
    keys <- mapM (\(KeyName k) -> do
        (key, val) <- showKey root p k
        pure $ "\"" <> key <> "\"=" <> val <> "\n"
        ) ks
    pure $ path <> "\n" <> BS.concat keys <> "\n"

showPath :: FilePath -> FilePath -> ByteString
showPath root p = "[ " <> BC.pack (makeRelative root p) <> " ]"

showKey :: FilePath -> FilePath -> FilePath -> IO (ByteString, ByteString)
showKey root p k = do
    val <- readKey (root </> p </> k)
    pure (BC.pack (takeBaseName k), BL.toStrict (val2reg val))

val2reg :: Value -> BL.ByteString
val2reg (REG_NONE v)                       = "\"" <> BL.fromStrict (BS.init v) <> "\""
val2reg (REG_SZ v)                         = "\"" <> BL.fromStrict (BS.init v) <> "\""
val2reg (REG_EXPAND_SZ v)                  = "\"" <> BL.fromStrict (BS.init v) <> "\""
val2reg (REG_BINARY v)                     = "hex:" <> BL.fromStrict v
val2reg (REG_DWORD v)                      = "dword:" <> toLazyByteString (word32HexFixed v)
val2reg (REG_DWORD_LITTLE_ENDIAN v)        = "dword:" <> toLazyByteString (word32HexFixed v)
val2reg (REG_DWORD_BIG_ENDIAN v)           = "dwordBE:" <> toLazyByteString (word32BE v)
val2reg (REG_LINK v)                       = "\"" <> toLazyByteString (byteString $ encodeUtf8 v) <> "\""
val2reg (REG_MULTI_SZ v)                   = "hex(7):" <> toLazyByteString (byteString $ BS.intercalate "\0" $ toList v)
val2reg (REG_RESOURCE_LIST v)              = toLazyByteString $ byteString $ BS.intercalate "\0" $ toList v
val2reg (REG_FULL_RESOURCE_DESCRIPTION v)  = toLazyByteString $ byteString $ BS.intercalate "\0" $ toList v
val2reg (REG_RESOURCE_REQUIREMENTS_LIST v) = toLazyByteString $ byteString $ BS.intercalate "\0" $ toList v
val2reg (REG_QWORD v)                      = "qword:" <> toLazyByteString (word64HexFixed v)
val2reg (REG_QWORD_LITTLE_ENDIAN v)        = "qword:" <> toLazyByteString (word64HexFixed v)
