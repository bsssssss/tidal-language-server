{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}

module Tidal.Log
    ( logToFile
    , logger
    , logFile
    , logMessage
    , LogLevel(..)) where

import           Control.Monad.IO.Class        (liftIO)
import qualified Data.Text                     as T
import           Data.Time
import qualified Language.LSP.Protocol.Message as LSP
import qualified Language.LSP.Protocol.Types   as LSP
import           Language.LSP.Server

class Monad m => Logger m where
    logMessage :: String -> LogLevel -> m ()

instance Logger IO where
    logMessage = logToFile

instance Logger (LspM ()) where
    logMessage = logger

data LogLevel = Log | Info | Warning | Error
    deriving (Show, Eq, Ord)

logger :: String -> LogLevel -> LspM () ()
logger msg level = do
    case level of
        Log -> liftIO $ logToFile msg level
        _   -> sendNotification LSP.SMethod_WindowShowMessage $ LSP.ShowMessageParams
                { _type_ = toLspMessageType level
                , _message = T.pack msg
                }

logToFile :: String -> LogLevel -> IO ()
logToFile msg level = do
    timestamp <- getFormatedTime
    appendFile logFile $
        "["++timestamp++"] " ++ toLogLevelString level ++ msg ++"\n"


toLogLevelString :: LogLevel -> String
toLogLevelString level =
    case level of
        Log     -> "[LOG]   "
        Info    -> "[INFO]  "
        Warning -> "[WARN]  "
        Error   -> "[ERROR] "


toLspMessageType :: LogLevel -> LSP.MessageType
toLspMessageType = \case
    Log     -> LSP.MessageType_Log
    Info    -> LSP.MessageType_Info
    Warning -> LSP.MessageType_Warning
    Error   -> LSP.MessageType_Error

getFormatedTime :: IO String
getFormatedTime = do
    tz <- getCurrentTimeZone
    utcTime <- getCurrentTime
    let localtime = utcToLocalTime tz utcTime
    return $ formatTime defaultTimeLocale "%H:%M:%S" localtime

logFile :: FilePath
logFile = "/tmp/tidal_ls.log"
