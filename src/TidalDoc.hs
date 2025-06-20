{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module TidalDoc
    ( testPath
    , FunctionInfo (..)
    , findTidalFunction
    , testFindFunction
    , collectDocumentation
    , tidalDocsVar
    , formatTidalFunction
    ) where

import           Control.Concurrent.STM      (newTVarIO, readTVarIO)
import           Control.Concurrent.STM.TVar (TVar)
import           Control.Exception           (IOException, catch)
import           Control.Monad.IO.Class      (liftIO)
import           Data.Char                   (isSpace)
import           Data.List                   (find)
import           Data.Maybe                  (mapMaybe)
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           GHC.IO                      (unsafePerformIO)
import           Language.Haskell.Exts
import           Log                         (LogLevel (..), logToFile)
import           System.Directory
import           System.Environment          (getEnv)
import           System.FilePath
import           Text.FuzzyFind              (Alignment, bestMatch, fuzzyFind)


---------------------------------------------------------------------------------
-- Get the files

getTidalSrcDir :: IO (Maybe FilePath)
getTidalSrcDir = catch (Just <$> getEnv "TIDAL_PATH") handleMissingEnv
    where
        handleMissingEnv :: IOException -> IO (Maybe FilePath)
        handleMissingEnv _ = return Nothing

resolveTidalFile :: FilePath -> IO FilePath
resolveTidalFile filename = do
    maybeDir <- getTidalSrcDir
    case maybeDir of
        Just dir -> return (dir </> filename)
        Nothing  -> error "TIDAL_PATH not set?"

getTidalSourceFiles :: IO [FilePath]
getTidalSourceFiles = do
    maybeDir <- getTidalSrcDir
    case maybeDir of
        Just dir -> do
            files <- listDirectory dir
            let excludedFiles = ["Utils.hs", "Show.hs", "TH.hs", "ParseBP.hs", "Listener.hs", "ID.hs","Context.hs"]
            let selectedFiles = filter (\f -> notElem (takeFileName f) excludedFiles && takeExtension f == ".hs") files
            return $ map (\f -> dir </> f) selectedFiles
        Nothing -> return []

tidalExtensions :: [Extension]
tidalExtensions =
    [ EnableExtension ExplicitForAll
    , EnableExtension GADTs
    , EnableExtension LambdaCase
    , EnableExtension BangPatterns
    , EnableExtension MultiParamTypeClasses
    , EnableExtension FlexibleContexts
    , EnableExtension InstanceSigs
    ]

---------------------------------------------------------------------------------
-- Core

data FunctionInfo = FunctionInfo
    { functionName :: String
    , functionDocs :: String
    , functionType :: String
    } deriving (Show, Eq)

tidalDocsVar :: TVar [FunctionInfo]
tidalDocsVar = unsafePerformIO $ do
    docs <- collectDocumentation
    newTVarIO docs
{-# NOINLINE tidalDocsVar #-}

collectDocumentation :: IO [FunctionInfo]
collectDocumentation = do
    files <- getTidalSourceFiles
    functionInfos <- mapM collectFunctionsFromFile files
    return (concat functionInfos)

collectFunctionsFromFile :: FilePath -> IO [FunctionInfo]
collectFunctionsFromFile path = do
    putStrLn $ "Parsing -> " ++ path
    content <- readFile path
    let parseMode = defaultParseMode
            { parseFilename = path
            , extensions = tidalExtensions }
    case parseModuleWithComments parseMode content of
        ParseOk (parsedModule, comments) -> do
            let annotatedAst = associateHaddock (parsedModule, comments)
                decls = case annotatedAst of
                        Module _ _ _ _ decls' -> decls'
                        _                     -> []
            return $ mapMaybe toFunctionInfo decls
        ParseFailed _ err -> error err

toFunctionInfo :: Decl (a, [Comment]) -> Maybe FunctionInfo
toFunctionInfo (TypeSig (_, comments) names type') = do
    let docs = formatDocs comments

    Just FunctionInfo
        { functionName = unwords $ map prettyPrint names
        , functionDocs = docs
        , functionType = "```haskell\n" ++ normalizeWhitespace (prettyPrint type') ++ "\n```"
        }
    where
        formatDocs :: [Comment] -> String
        formatDocs = unlines . map (cleanComment . (\(Comment _ _ txt) -> txt))

        cleanComment txt = case dropWhile isSpace txt of
            -- '|':rest -> dropWhile isSpace rest
            '>':rest -> dropWhile isSpace rest
            other    -> other

toFunctionInfo _ = Nothing

normalizeWhitespace :: String -> String
normalizeWhitespace = unwords . words

---------------------------------------------------------------------------------
-- Find a function

findTidalFunction :: String -> IO (Maybe FunctionInfo)
findTidalFunction funcName = do
    -- docs <- collectDocumentation
    docs <- readTVarIO tidalDocsVar
    return $ lookupFunction funcName docs

lookupFunction :: String -> [FunctionInfo] -> Maybe FunctionInfo
lookupFunction funcName = find (\doc -> functionName doc == funcName)

formatTidalFunction :: FunctionInfo -> Text
formatTidalFunction FunctionInfo{..} = T.pack $
            functionName ++ "\n" ++
            functionType ++ "\n" ++
            replicate (length functionName) '-' ++ "\n\n" ++
            "  " ++ functionDocs ++ "\n\n"

---------------------------------------------------------------------------------
-- print

printFunctionInfo :: FunctionInfo -> IO ()
printFunctionInfo FunctionInfo {..} = do
    putStrLn $ functionName ++ " :: " ++ functionType
    putStrLn $ unlines $ map ("  " ++) $ lines functionDocs


---------------------------------------------------------------------------------
-- test

testFindFunction :: String -> IO ()
testFindFunction funcName = do
    result <- findTidalFunction funcName
    case result of
        Just functionInfo -> printFunctionInfo functionInfo
        Nothing -> putStrLn $ "Function '" ++ funcName ++ "' not found !"

testPath :: FilePath -> IO ()
testPath filename = do
    path <- case filename of
        "test" -> return "test/TestMe.hs"
        _      -> resolveTidalFile filename
    putStrLn $ "\nGathering " ++ path ++ "...\n"
    functionInfos <- collectFunctionsFromFile path
    if null functionInfos
        then putStrLn "Nothing to document !"
        else do
            putStrLn $ replicate 80 '-'
            putStrLn $ "Found " ++ show (length functionInfos) ++ " documented functions!"
            putStrLn $ replicate 80 '-' ++ "\n"
            mapM_ printFunctionInfo functionInfos


writeDocsToFile :: IO ()
writeDocsToFile = do
    docs <- collectDocumentation
    writeFile "/tmp/tidal-docs.txt" (concatMap formatDocs docs)
    where
        formatDocs FunctionInfo{..} =
            functionName ++ "\n" ++
            replicate (length functionName) '-' ++ "\n" ++
            "Type: " ++ functionType ++ "\n\n" ++
            functionDocs ++ "\n\n"
