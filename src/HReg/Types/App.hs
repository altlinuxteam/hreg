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

module HReg.Types.App (
    Config(..),
    Scope(..),
    HasLog(..),
    HasPath(..),
    HasPolFiles(..),
    HasRegFiles(..),
    App(..),
    runApp
) where

import Control.Monad.Catch
import qualified Logger as Log
import HReg.Types.Reg

data Scope = Machine
           | User Text
    deriving (Eq, Show)

data Config = Config{ dataPath :: FilePath
                    , polFiles :: [(FilePath, Scope)]
                    , regFiles :: [FilePath]
                    }
    deriving (Eq, Show)

newtype App env a = App {unApp :: ReaderT env IO a}
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader env, MonadThrow, MonadCatch)

class HasPath a where
    getPath :: a -> FilePath

class HasLog a where
    logInfo :: (HasCallStack, MonadIO m) => a -> (Text -> m ())
    logWarning :: (HasCallStack, MonadIO m) => a -> (Text -> m ())
    logError :: (HasCallStack, MonadIO m) => a -> (Text -> m ())

class HasPolFiles a where
    getPolFiles :: a -> [(FilePath, Scope)]

class HasRegFiles a where
    getRegFiles :: a -> [FilePath]

instance HasPath Config where
    getPath = dataPath

instance HasLog Config where
    logInfo _a = Log.log callStack Log.Info
    logWarning _a = Log.log callStack Log.Warning
    logError _a = Log.log callStack Log.Error

instance HasPolFiles Config where
    getPolFiles = polFiles

instance HasRegFiles Config where
    getRegFiles = regFiles

runApp :: MonadIO m => env -> App env a -> m a
runApp env (App (ReaderT f)) = liftIO (f env)

