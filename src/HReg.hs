-----------------------------------------------------------------------------
--
-- Module      :  HReg
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

module HReg (
    runApp,
    hregApply,
    hregExport,
    hregImport,
    Config(..),
    Scope(..),
    readKey
--    hregCli,
--    module HReg
) where

--import HReg.CLI as HReg

import Data.Binary.Get
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import Data.ByteString.Builder
import Data.Text (pack, unpack, splitOn)
import Data.Text.Encoding (decodeUtf16LE, encodeUtf16LE)
import qualified Data.Text as T
import HReg.Types
import HReg.Polfiles (parse, Action(..), Key(..), Value(..))
import HReg.Regfiles.Parser
import           HReg.Regfiles.Types
import System.Directory
import System.FilePath ((</>), (<.>), takeFileName, takeDirectory, takeExtension)

hregExport :: (HasCallStack, HasPath env, HasLog env, HasPolFiles env) => App env ()
hregExport = undefined {-do
    env <- ask
    let root = getPath env
    processPolFiles
-}

hregImport :: (HasCallStack, HasPath env, HasLog env, HasRegFiles env) => App env ()
hregImport = importRegFile

importRegFile :: (HasCallStack, HasLog env, HasRegFiles env, HasPath env) => App env ()
importRegFile = do
    env <- ask
    let root = getPath env
        regs = getRegFiles env
    mapM_ (\f -> do
        logInfo env $ "Parse '" <> T.pack f <> "' file"
        recs <- liftIO $ parseReg f
        case recs of
            Left e -> do
                logError env $ "failed to parse '" <> T.pack f <> "' as a .reg file"
                error "regfile parser failed"
            Right rs -> mapM_ writeRec rs
        logInfo env "Done"
        ) regs

writeRec :: (HasCallStack, HasPath env, HasLog env) => RegItem -> App env ()
writeRec (Path p, keys) = do
    env <- ask
    let root = getPath env
        path = key2path (T.tail . T.init . T.pack $ p)
    logInfo env $ "create directory for '" <> T.pack path <> "'"
    liftIO $ createDirectoryPath root path
    mapM_ (\(KeyName k, v) -> do
        logInfo env $ "write key '" <> T.pack k <> "'"
        writeRegKey (root </> path) k v
        ) keys

hregApply :: (HasCallStack, HasPath env, HasLog env, HasPolFiles env) => App env ()
hregApply = do
    env <- ask
    let root = getPath env
    processPolFiles


processPolFiles :: (HasCallStack, HasLog env, HasPolFiles env, HasPath env) => App env ()
processPolFiles = do
    env <- ask
    let root = getPath env
    forM_ (getPolFiles env)
        (\(p, s) -> do
            logInfo env $ "Process '" <> pack p <> "' file in '" <> show s <> "' scope"
            cont <- liftIO $ BL.readFile p
            let path = case s of
                        Machine  -> root </> "HKEY_LOCAL_MACHINE"
                        User sid -> root </> "HKEY_USERS" </> unpack sid
            mapM_ (applyAction path) $ parse cont
        )

applyAction :: (HasCallStack, HasLog env) => FilePath -> Action -> App env ()
applyAction root (SetValue k v) = do
    env <- ask
    logInfo env $ "Applying action: '" <> show k <> "'"
    let keyPath = key2path k
        path    = takeDirectory keyPath
        keyFile = takeFileName keyPath
    logInfo env $ "Create directory: '" <> pack path <> "'"
    liftIO $ createDirectoryPath root path
    writeKey (root </> path) keyFile v

applyAction _ act = do
    env <- ask
    logInfo env $ "Unimplemented action: '" <> show act <> "'"

writeKey :: (HasCallStack, HasLog env) => FilePath -> FilePath -> Value -> App env ()
writeKey p k v = do
    env <- ask
    logInfo env $ "Write key '" <> pack k <> "' with '" <> show v <> "' value"
    let (fname, valBS) = val2file k v
    liftIO $ BL.writeFile (p </> fname) (toLazyByteString valBS)

writeRegKey :: (HasCallStack, HasLog env) => FilePath -> FilePath -> Value -> App env ()
writeRegKey p k v = do
    env <- ask
    logInfo env $ "Write reg key '" <> pack k <> "'"
    let (fname, valBS) = (k <.> (type2ext v), encodeDataOnly v)
    liftIO $ BL.writeFile (p </> fname) (BL.fromStrict valBS)

type2ext :: Value -> String
type2ext (REG_NONE _)                       = ""
type2ext (REG_SZ _)                         = "sz"
type2ext (REG_EXPAND_SZ _)                  = "esz"
type2ext (REG_BINARY _)                     = "bin"
type2ext (REG_DWORD _)                      = "dw"
type2ext (REG_DWORD_LITTLE_ENDIAN _)        = "dwle"
type2ext (REG_DWORD_BIG_ENDIAN _)           = "dwbe"
type2ext (REG_LINK _)                       = "link"
type2ext (REG_MULTI_SZ _)                   = "msz"
type2ext (REG_RESOURCE_LIST _)              = "rl"
type2ext (REG_FULL_RESOURCE_DESCRIPTION _)  = "frd"
type2ext (REG_RESOURCE_REQUIREMENTS_LIST _) = "rrq"
type2ext (REG_QWORD _)                      = "qw"
type2ext (REG_QWORD_LITTLE_ENDIAN _)        = "qwle"

val2file :: FilePath -> Value -> (FilePath, Builder)
val2file k (REG_NONE v)                       = (k           , byteString v)
val2file k (REG_SZ v)                         = (k <.> "sz"  , byteString $ utf16toUtf8 v)
val2file k (REG_EXPAND_SZ v)                  = (k <.> "esz" , byteString $ utf16toUtf8 v)
val2file k (REG_BINARY v)                     = (k <.> "bin" , byteString v)
val2file k (REG_DWORD v)                      = (k <.> "dw"  , word32LE v)
val2file k (REG_DWORD_LITTLE_ENDIAN v)        = (k <.> "dwle", word32LE v)
val2file k (REG_DWORD_BIG_ENDIAN v)           = (k <.> "dwbe", word32BE v)
val2file k (REG_LINK v)                       = (k <.> "link", byteString $ encodeUtf8 v)
val2file k (REG_MULTI_SZ v)                   = (k <.> "msz" , byteString $ BS.intercalate "\0" $ toList v)
val2file k (REG_RESOURCE_LIST v)              = (k <.> "rl"  , byteString v)
val2file k (REG_FULL_RESOURCE_DESCRIPTION v)  = (k <.> "frd" , byteString v)
val2file k (REG_RESOURCE_REQUIREMENTS_LIST v) = (k <.> "rrq" , byteString v)
val2file k (REG_QWORD v)                      = (k <.> "qw"  , word64LE v)
val2file k (REG_QWORD_LITTLE_ENDIAN v)        = (k <.> "qwle", word64LE v)

readKey :: FilePath -> IO Value
readKey fp = do
    bs <- BS.readFile fp
    let ext = takeExtension fp
        strz = BS.snoc bs 0

    pure $ case ext of
        ".sz"   -> REG_SZ strz
        ".esz"  -> REG_EXPAND_SZ strz
        ".bin"  -> REG_BINARY bs
        ".dw"   -> REG_DWORD $ runGet getWord32le $ BL.fromStrict bs
        ".dwle" -> REG_DWORD_LITTLE_ENDIAN $ runGet getWord32le $ BL.fromStrict bs
        ".dwbe" -> REG_DWORD_BIG_ENDIAN $ runGet getWord32be $ BL.fromStrict bs
        ".link" -> REG_LINK $ decodeUtf8 bs
        ".msz"  -> REG_MULTI_SZ $ fromList $ BS.split 0 bs
        ".rl"   -> REG_RESOURCE_LIST bs
        ".frd"  -> REG_FULL_RESOURCE_DESCRIPTION bs
        ".rrq"  -> REG_RESOURCE_REQUIREMENTS_LIST bs
        ".qw"   -> REG_QWORD $ runGet getWord64le $ BL.fromStrict bs
        ".qwle" -> REG_QWORD_LITTLE_ENDIAN $ runGet getWord64le $ BL.fromStrict bs
        _       -> REG_NONE bs -- maybe it should fail in case of unknown extension?

utf16toUtf8 :: ByteString -> ByteString
utf16toUtf8 = encodeUtf8 . decodeUtf16LE

key2path :: Key -> FilePath
key2path k = unpack $ T.map (\c -> if c == '\\' then '/' else c) k

createDirectoryPath :: FilePath -> FilePath -> IO ()
createDirectoryPath root dp = createDirectoryIfMissing True (root </> dp)

