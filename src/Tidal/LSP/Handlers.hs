{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}

module Tidal.LSP.Handlers (handlers) where

import qualified Language.LSP.Protocol.Message as LSP
import qualified Language.LSP.Protocol.Types   as LSP
import           Language.LSP.Server
import           Tidal.Log                     (LogLevel (..), logMessage)
import           Tidal.LSP.Handlers.Completion (handleCompletion)
import           Tidal.LSP.Handlers.Hover      (handleHover)

handlers :: Handlers (LspM ())
handlers = do
    mconcat
        [ notificationHandler LSP.SMethod_Initialized $ \_not -> do
            -- logMessage "Language server initialized" Info
            pure ()

        , notificationHandler LSP.SMethod_TextDocumentDidOpen $ \_not -> do
            let LSP.TNotificationMessage _ _ params = _not  -- Note the T prefix
                LSP.DidOpenTextDocumentParams {_textDocument} = params  -- This extracts the params
            -- logMessage "Got didOpen notification" Info
            -- logMessage ("TextDocument: " ++ show _textDocument) Log
            sendNotification LSP.SMethod_WindowShowMessage $ LSP.ShowMessageParams
                { _type_ = LSP.MessageType_Info
                , _message = "Hello Tidalist :)"
                }

        , notificationHandler LSP.SMethod_TextDocumentDidChange $ \_ -> do
            pure ()

        , notificationHandler LSP.SMethod_TextDocumentDidSave $ \_ -> do
            pure ()

        , notificationHandler LSP.SMethod_WorkspaceDidChangeConfiguration $ \_ -> do
            pure ()

        , requestHandler LSP.SMethod_TextDocumentHover $ \req responder -> do
            rsp <- handleHover req
            responder rsp

        , requestHandler LSP.SMethod_TextDocumentCompletion $ \req responder -> do
            rsp <- handleCompletion req
            responder rsp
        ]
