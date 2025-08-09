{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE TypeOperators         #-}

module Tidal.LSP.Handlers.Completion (handleCompletion) where

import           Control.Concurrent.STM        (readTVarIO)
import           Control.Lens                  hiding (Iso)
import           Control.Monad.IO.Class
import           Data.List                     (sortOn)
import qualified Data.Text                     as T
import qualified Language.LSP.Protocol.Message as LSP
import qualified Language.LSP.Protocol.Types   as LSP
import           Language.LSP.Server
import           Text.FuzzyFind
import           Tidal.Documentation           (FunctionInfo (..),
                                                tidalDocumentation)
import           Tidal.LSP.Document            (DocInfo (..), getWordAtPos,
                                                mkDocInfo, readDoc)

type CompletionResult = Either (LSP.TResponseError LSP.Method_TextDocumentCompletion) ([LSP.CompletionItem] LSP.|? (LSP.CompletionList LSP.|? LSP.Null))

handleCompletion :: LSP.TRequestMessage LSP.Method_TextDocumentCompletion -> LspM () CompletionResult
handleCompletion req = do
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
    tidalDocumentation' <- liftIO $ readTVarIO tidalDocumentation

    case word of
        Nothing -> return $ Right $ LSP.InL $ List []
        Just w -> do
            let matches = [( functionName tidalDoc, match, tidalDoc)
                           | tidalDoc <- tidalDocumentation'
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
                        , _documentation       = Just $ LSP.InL $ T.pack (functionDocumentation tidalDoc)
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
            return $ Right $ LSP.InL $ List completionItems

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
