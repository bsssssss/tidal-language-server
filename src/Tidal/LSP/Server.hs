{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module Tidal.LSP.Server (server) where

import           Control.Monad.IO.Class
import qualified Language.LSP.Protocol.Types as LSP
import           Language.LSP.Server
import           Tidal.Log                   (LogLevel (..), logFile,
                                              logMessage)
import           Tidal.LSP.Handlers          (handlers)

server :: IO Int
server = do
    writeFile logFile ""
    logMessage "Starting Server.." Info
    runServer $
        ServerDefinition
              { parseConfig = const $ const $ Right ()
              , onConfigChange = const $ pure ()
              , defaultConfig = ()
              , configSection = "demo"
              , doInitialize = \env _req -> do
                    pure $ Right env
              , staticHandlers = \_caps -> handlers
              , interpretHandler = \env -> Iso (runLspT env) liftIO
              , options = lspOptions
              }

syncOptions :: LSP.TextDocumentSyncOptions
syncOptions =
    LSP.TextDocumentSyncOptions
        { _openClose = Just True
        , _change = Just LSP.TextDocumentSyncKind_Incremental
        , _willSave = Just False
        , _willSaveWaitUntil = Just False
        , _save = Just $ LSP.InR $ LSP.SaveOptions $ Just False
        }

lspOptions :: Options
lspOptions =
    defaultOptions
        { optTextDocumentSync = Just syncOptions
        , optCompletionTriggerCharacters = Just $ ['a'..'z']++['A'..'Z']
        }
