-----------------------------------------------------------------------------
--
-- Module      :  Logger
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

module Logger (
    logInfo,
    log,
    Severity(..)
) where

import Colog hiding (log)

import Control.Monad.IO.Class
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO

processLinesLog :: (WithLog env Message m, MonadIO m) => m ()
processLinesLog = do
  logInfo "Enter some string"
  line <- liftIO TextIO.getLine
  case Text.length line of
    0 -> do
      logWarning "Empty input"
      processLinesLog
    n -> do
      logDebug "Correct line"
      logInfo $ "Line length: " <> Text.pack (show n)

--withLogger :: (WithLog env Message m, MonadIO m) => m ()
withLogger a = let action = richMessageAction --cmap fmtMessage logTextStdout
  in usingLoggerT action a

logD :: (HasCallStack, MonadIO m) => Text -> m()
logD = log callStack Debug
logI :: (HasCallStack, MonadIO m) => Text -> m()
--logI :: WithLog env Message m => Text -> m ()
logI = log callStack Info
logW :: (HasCallStack, MonadIO m) => Text -> m()
--logW :: WithLog env Message m => Text -> m ()
logW = log callStack Warning
logE :: (HasCallStack, MonadIO m) => Text -> m()
--logE :: WithLog env Message m => Text -> m ()
logE = log callStack Error
{-
log :: (HasCallStack, MonadIO m) => CallStack -> Severity -> Text -> m ()
log cs s msg = fmt <& (Msg s cs msg)
  where fmt = contramap fmtMessage logTextStdout
-}
log :: (HasCallStack, MonadIO m) => CallStack -> Severity -> Text -> m ()
log cs s msg = richMessageAction <& (Msg s cs msg)
