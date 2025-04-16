---------------------------------------------------------------------------------

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE LambdaCase            #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use isJust" #-}

module Main
    ( main
    )
    where

---------------------------------------------------------------------------------

import           Control.Concurrent.STM        (readTVarIO)
import           Control.Lens                  hiding (Iso)
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.List                     (sortOn)
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import qualified Language.LSP.Protocol.Lens    as LSP
import qualified Language.LSP.Protocol.Message as LSP
import qualified Language.LSP.Protocol.Types   as LSP
import           Language.LSP.Server
import           Language.LSP.VFS
import           Log                           (LogLevel (..), logFile,
                                                logToFile, lspLogger)
import           System.Directory              (doesFileExist)
import           Text.FuzzyFind
import           TidalDoc                      (FunctionInfo (..),
                                                collectDocumentation,
                                                findTidalFunction,
                                                formatTidalFunction,
                                                tidalDocsVar)


---------------------------------------------------------------------------------
-- server

main :: IO Int
main = do
    writeFile logFile ""
    _ <- liftIO $ logToFile "Starting Server.." Info
    runServer $
        ServerDefinition
              { parseConfig = const $ const $ Right ()
              , onConfigChange = const $ pure ()
              , defaultConfig = ()
              , configSection = "demo"
              , doInitialize = \env _req -> do
                    -- logDebug $ "Server init request received"
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
        , LSP._save = Just $ LSP.InR $ LSP.SaveOptions $ Just False
        }

lspOptions :: Options
lspOptions =
    defaultOptions
        { optTextDocumentSync = Just syncOptions
        , optCompletionTriggerCharacters = Just $
            ['a'..'z']++['A'..'Z']
        }

---------------------------------------------------------------------------------
-- lsp

handlers :: Handlers (LspM ())
handlers = do
    mconcat
        [ notificationHandler LSP.SMethod_Initialized $ \_not -> do
            -- docs <- liftIO $ readTVarIO tidalDocsVar
            lspLogger "Language server initialized" Info

        , notificationHandler LSP.SMethod_TextDocumentDidOpen $ \_not -> do
            let LSP.TNotificationMessage _ _ params = _not  -- Note the T prefix
                LSP.DidOpenTextDocumentParams {_textDocument} = params  -- This extracts the params
            liftIO $ logToFile "Got didOpen notification" Info
            -- liftIO $ logToFile ("TextDocument: " ++ show _textDocument) Log
            sendNotification LSP.SMethod_WindowShowMessage $ LSP.ShowMessageParams
                { _type_ = LSP.MessageType_Info
                , _message = "Hello Editor :)"
                }

        , notificationHandler LSP.SMethod_TextDocumentDidChange $ \msg -> do
            pure ()
            -- liftIO $ logToFile "Got DidChange message" Info
            -- let doc =
            --       msg
            --         ^. LSP.params
            --           . LSP.textDocument
            --           . LSP.uri
            --           . to LSP.toNormalizedUri
            -- mdoc <- getVirtualFile doc
            -- case mdoc of
            --   Just (VirtualFile _lsp_version _file_version _file_text) ->
            --     liftIO $ logToFile ("Change in virtual file : " ++ show mdoc) Log
            --   Nothing -> do
            --     liftIO $ logToFile ("Didn't find file in VFS: " ++ show doc) Warning

        , requestHandler LSP.SMethod_TextDocumentHover $ \req responder -> do
            -- liftIO $ logToFile "received textDocument/hover request" Info
            let LSP.TRequestMessage _ _ _ params = req
                LSP.HoverParams _docId _pos _ = params
                docInfo = mkDocInfo _docId _pos
            doc <- readDoc docInfo
            rsp <- liftIO $ hoverResponse doc
            responder rsp

        , requestHandler LSP.SMethod_TextDocumentCompletion $ \req responder -> do
            -- liftIO $ logToFile "received textDocument/completion request" Info
            let LSP.TRequestMessage _ _ _ params = req
                LSP.CompletionParams _docId _position _ _ _ = params
                docInfo = mkDocInfo _docId _position
            doc <- readDoc docInfo
            -- liftIO $ logToFile ("Completion request for :\n" ++ show doc) Log
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
                    forM_ sortedMatches $ \(name, _, tidalDoc) ->
                        liftIO $ logToFile ("[MARkDOWN DEBUG] Generated docs:\n" ++ functionDocs tidalDoc) Log

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
            -- maybeFunc <- findTidalFunction (T.unpack word)
            maybeFunc <- findTidalFunction (T.unpack word)
            return $ maybe defaultMsg formatTidalFunction maybeFunc
        defaultMsg :: Text
        defaultMsg = "No information available"

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


---------------------------------------------------------------------------------
-- Editor's Doc

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
            -- liftIO $ logToFile ("Updated content: " ++ content) Log
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

---------------------------------------------------------------------------------
