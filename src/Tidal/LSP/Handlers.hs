{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}

{-# OPTIONS_GHC -Wno-name-shadowing       #-}

module Tidal.LSP.Handlers (handlers) where

import           Control.Concurrent.STM        (readTVarIO)
import           Control.Lens                  hiding (Iso)
import           Control.Monad.IO.Class
import           Data.List                     (sortOn)
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import qualified Language.LSP.Protocol.Message as LSP
import qualified Language.LSP.Protocol.Types   as LSP
import           Language.LSP.Server
import           Log                           (LogLevel (..), logFile,
                                                logToFile, lspLogger)
import           Text.FuzzyFind
import           Tidal.LSP.Document            (DocInfo (..), mkDocInfo, readDoc, getWordAtPos)
import           TidalDoc                      (FunctionInfo (..),
                                                findTidalFunction,
                                                formatTidalFunction,
                                                tidalDocsVar)

handlers :: Handlers (LspM ())
handlers = do
    mconcat
        [ notificationHandler LSP.SMethod_Initialized $ \_not -> do
            -- docs <- liftIO $ readTVarIO tidalDocsVar
            lspLogger "Language server initialized" Info

        , notificationHandler LSP.SMethod_TextDocumentDidOpen $ \_not -> do
            let LSP.TNotificationMessage _ _ params = _not  -- Note the T prefix
                LSP.DidOpenTextDocumentParams {_textDocument} = params  -- This extracts the params
            -- liftIO $ logToFile "Got didOpen notification" Info
            -- liftIO $ logToFile ("TextDocument: " ++ show _textDocument) Log
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
            let LSP.TRequestMessage _ _ _ params = req
                LSP.HoverParams _docId _pos _ = params
                docInfo = mkDocInfo _docId _pos
            doc <- readDoc docInfo
            rsp <- liftIO $ hoverResponse doc
            responder rsp

        , requestHandler LSP.SMethod_TextDocumentCompletion $ \req responder -> do
            let LSP.TRequestMessage _ _ _ params = req
                LSP.CompletionParams _docId _position _ _ _ = params
                docInfo = mkDocInfo _docId _position
            doc <- readDoc docInfo

            let DocInfo _ _ _pos _text _ = doc
            word <- case docContent doc of
                Nothing -> return Nothing
                Just content -> do
                    let maybeWord = getWordAtPos content _pos
                    return maybeWord
            tidalDocs <- liftIO $ readTVarIO tidalDocsVar

            case word of
                Nothing -> responder $ Right $ LSP.InL $ List []
                Just w -> do
                    let matches = [( functionName tidalDoc, match, tidalDoc)
                                   | tidalDoc <- tidalDocs
                                   , let match = customMatch (T.unpack w) (functionName tidalDoc)
                                   , match /= Nothing
                                  ]
                        getScore (_, Just alignment, _) = score alignment
                        getScore (_, Nothing, _)        = 0

                        sortedMatches = sortOn (negate . getScore) matches
                    -- forM_ sortedMatches $ \(name, _, tidalDoc) ->
                    --     liftIO $ logToFile ("[MARKDOWN DEBUG] Generated docs:\n" ++ functionDocs tidalDoc) Log

                    let completionItems =
                            [ LSP.CompletionItem
                                { _label               = T.pack name
                                , _kind                = Just LSP.CompletionItemKind_Function
                                , _documentation       = Just $ LSP.InL $ T.pack (functionDocs tidalDoc)
                                , _detail              = Just $ T.pack (functionType tidalDoc)
                                , _labelDetails        = Nothing
                                , _tags                = Nothing
                                , _deprecated          = Nothing
                                , _preselect           = Nothing
                                , _sortText            = Nothing
                                , _filterText          = Nothing
                                , _insertText          = Nothing
                                , _insertTextFormat    = Nothing
                                , _insertTextMode      = Nothing
                                , _textEdit            = Nothing
                                , _textEditText        = Nothing
                                , _additionalTextEdits = Nothing
                                , _commitCharacters    = Nothing
                                , _command             = Nothing
                                , _data_               = Nothing
                                }
                                | (name, _, tidalDoc) <- sortedMatches
                            ]
                    responder $ Right $ LSP.InL $ List completionItems
        ]

type HoverResult = Either (LSP.TResponseError LSP.Method_TextDocumentHover) (LSP.Hover LSP.|? LSP.Null)

customMatch :: String -> String -> Maybe Alignment
customMatch = bestMatch'
    defaultMatchScore
    defaultMismatchScore
    (defaultGapPenalty * 2)
    defaultBoundaryBonus
    0
    2
    defaultConsecutiveBonus
    0

hoverResponse :: DocInfo -> IO HoverResult
hoverResponse doc = do
    wordInfo <- case docContent doc of
        Nothing -> return defaultMsg
        Just content ->
            case getWordAtPos content (docCursorPos doc) of
                Nothing   -> return defaultMsg
                Just word -> getFunctionInfo word
    let
        pos = docCursorPos doc
        msg = LSP.mkMarkdown wordInfo
        rng = LSP.Range pos pos
        rsp = LSP.Hover (LSP.InL msg) (Just rng)
    return (Right $ LSP.InL rsp)
    where
        getFunctionInfo :: Text -> IO Text
        getFunctionInfo word = do
            maybeFunc <- findTidalFunction (T.unpack word)
            return $ maybe defaultMsg formatTidalFunction maybeFunc
        defaultMsg :: Text
        defaultMsg = "No information available"

