{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}

module Tidal.LSP.Document
    ( DocInfo (..)
    , DocResult
    , mkDocInfo
    , readDoc
    , getWordAtPos
    ) where

import           Control.Monad.IO.Class
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import qualified Language.LSP.Protocol.Types as LSP
import           Language.LSP.Server
import           Language.LSP.VFS            hiding (line)
import           Prelude                     hiding (getLine)
import           System.Directory            (doesFileExist)

data DocInfo = DocInfo
    { docUri       :: LSP.Uri
    , docPath      :: Maybe FilePath
    , docCursorPos :: LSP.Position
    , docContent   :: Maybe Text
    , version      :: Maybe Int
    } deriving (Show)

data DocResult
    = DocNotFound  Text
    | DocReadError FilePath
    | DocOk        DocInfo
    deriving (Show)

mkDocInfo :: LSP.TextDocumentIdentifier -> LSP.Position -> DocInfo
mkDocInfo docId pos =
    DocInfo
        { docUri       = uri
        , docPath      = LSP.uriToFilePath uri
        , docCursorPos = pos
        , docContent   = Nothing
        , version      = Nothing
        } where
            LSP.TextDocumentIdentifier uri = docId

readDoc :: DocInfo -> LspM () DocInfo
readDoc doc = do
    let nuri = LSP.toNormalizedUri (docUri doc)
    mvf <- getVirtualFile nuri
    case mvf of
        Just (VirtualFile _lsp_version _file_version _file_text) -> do
            let content = T.unpack $ virtualFileText $ VirtualFile _lsp_version _file_version _file_text
            pure $ doc { docContent = Just $ T.pack content, version = Just _file_version }
        Nothing ->  case docPath doc of
            Nothing -> do
                pure doc
            Just path -> do
                fileExists <- liftIO $ doesFileExist path
                if not fileExists
                then pure doc
                else do
                    content <- liftIO $ readFile path
                    pure $ doc { docContent = Just $ T.pack content }

getWordAtPos :: Text -> LSP.Position -> Maybe Text
getWordAtPos content LSP.Position{_line, _character} = do
    let lines' = T.lines content
    line' <- getLine (fromIntegral _line) lines'
    let word = getWord (fromIntegral _character) line'
    if T.null word
        then Nothing
        else Just word
    where
        isChar c = c `elem` (['a'..'z']++['A'..'Z']++['0'..'9']++['_'])

        getLine :: Int -> [Text] -> Maybe Text
        getLine lineNr lines' =
            if lineNr < 0 || lineNr >= fromIntegral (length lines')
                then Nothing
                else Just $ lines' !! lineNr

        getWord :: Int -> Text -> Text
        getWord pos line =
            let before     = T.take pos line
                after      = T.drop pos line
                beforeWord = T.takeWhileEnd isChar before
                afterWord  = T.takeWhile isChar after
            in  beforeWord <> afterWord
