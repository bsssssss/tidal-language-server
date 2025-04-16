{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use isJust" #-}

module Log
    ( logToFile
    , lspLogger
    , logFile
    , LogLevel(..)) where

import           Control.Monad.IO.Class        (liftIO)
import qualified Data.Text                     as T
import           Data.Time
import qualified Language.LSP.Protocol.Message as LSP
import qualified Language.LSP.Protocol.Types   as LSP
import           Language.LSP.Server

data LogLevel = Log | Info | Warning | Error
    deriving (Show, Eq, Ord)

lspLogger :: String -> LogLevel -> LspM () ()
lspLogger msg level = do
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
    tz  <- getCurrentTimeZone
    utc <- getCurrentTime
    let localtime = utcToLocalTime tz utc
    return $ formatTime defaultTimeLocale "%H:%M:%S" localtime

logFile :: FilePath
logFile = "/tmp/tidal_ls.log"
