{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}

module Tidal.LSP.Handlers.Hover (hoverResponse, handleHover) where

import           Control.Monad.IO.Class
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import qualified Language.LSP.Protocol.Message as LSP
import qualified Language.LSP.Protocol.Types   as LSP
import           Language.LSP.Server
import           Tidal.Documentation           (findTidalFunction,
                                                formatTidalFunction)
import           Tidal.LSP.Document            (DocInfo (..), getWordAtPos,
                                                mkDocInfo, readDoc)

type HoverResult = Either (LSP.TResponseError LSP.Method_TextDocumentHover) (LSP.Hover LSP.|? LSP.Null)

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

handleHover :: LSP.TRequestMessage LSP.Method_TextDocumentHover -> LspM () HoverResult
handleHover req = do
    let LSP.TRequestMessage _ _ _ params = req
        LSP.HoverParams _docId _pos _ = params
        docInfo = mkDocInfo _docId _pos
    doc <- readDoc docInfo
    liftIO $ hoverResponse doc
