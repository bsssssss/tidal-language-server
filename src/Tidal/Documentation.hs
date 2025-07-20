{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Tidal.Documentation
    ( testPath
    , FunctionInfo (..)
    , findTidalFunction
    , testFindFunction
    , collectDocumentation
    , tidalDocumentation
    , formatTidalFunction
    , writeDocsToFile
    ) where

import           Control.Concurrent.STM      (newTVarIO, readTVarIO)
import           Control.Concurrent.STM.TVar (TVar)
import           Control.Exception           (IOException, catch)
import           Data.Char                   (isSpace)
import           Data.List                   (find)
import           Data.Maybe                  (mapMaybe)
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           GHC.IO                      (unsafePerformIO)
import           Language.Haskell.Exts
import           System.Directory
import           System.Environment          (getEnv)
import           System.FilePath


-- | Information about a Tidal function including its name, documentation, and type signature
data FunctionInfo = FunctionInfo
    { functionName          :: String -- ^ The function name
    , functionDocumentation :: String -- ^ The Haddock documentation
    , functionType          :: String -- ^ The formatted type signature
    } deriving (Show, Eq)

-- | Get the Tidal source directory from the TIDAL_SRC_PATH environment variable
getTidalSrcDir :: IO (Maybe FilePath)
getTidalSrcDir = catch (Just . (</> "tidal-core/src/Sound/Tidal") <$> getEnv "TIDAL_SRC_PATH") handleMissingEnv
    where
        handleMissingEnv :: IOException -> IO (Maybe FilePath)
        handleMissingEnv _ = return Nothing

-- | Resolve a Tidal filename to its full path
resolveTidalFile :: FilePath -> IO FilePath
resolveTidalFile filename = do
    maybeDir <- getTidalSrcDir
    case maybeDir of
        Just dir -> return (dir </> filename)
        Nothing  -> error "TIDAL_SRC_PATH not in PATH"

-- | Get all Tidal source files, excluding certain utility files
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

-- | Language extensions needed to parse Tidal source files
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

-- | Global TVar containing cached Tidal documentation
tidalDocumentation :: TVar [FunctionInfo]
tidalDocumentation = unsafePerformIO $ do
    docs <- collectDocumentation
    newTVarIO docs
{-# NOINLINE tidalDocumentation #-}

-- | Collect documentation from all Tidal source files
collectDocumentation :: IO [FunctionInfo]
collectDocumentation = do
    files <- getTidalSourceFiles
    functionInfos <- mapM collectFunctionsFromFile files
    return (concat functionInfos)

-- | Parse a Haskell file and extract function documentation
collectFunctionsFromFile :: FilePath -> IO [FunctionInfo]
collectFunctionsFromFile path = do
    -- putStrLn $ "Parsing -> " ++ path
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
    let name' = unwords $ map prettyPrint names
    Just FunctionInfo
        { functionName = name'
        , functionDocumentation = docs
        , functionType = "```haskell\n" ++ name' ++ " :: " ++ normalizeWhitespace (prettyPrint type') ++ "\n```"
        }
    where
        formatDocs :: [Comment] -> String
        formatDocs = unlines . map (cleanComment . (\(Comment _ _ txt) -> txt))
        cleanComment txt = case dropWhile isSpace txt of
            '|':rest -> dropWhile isSpace rest
            '>':rest -> dropWhile isSpace rest
            other    -> other

toFunctionInfo _ = Nothing

-- | Normalize whitespace in a string by collapsing multiple spaces
normalizeWhitespace :: String -> String
normalizeWhitespace = unwords . words

-- | Find documentation for a specific Tidal function by name
findTidalFunction :: String -> IO (Maybe FunctionInfo)
findTidalFunction funcName = do
    -- docs <- collectDocumentation
    docs <- readTVarIO tidalDocumentation
    return $ lookupFunction funcName docs

-- | Lookup a function in a list of FunctionInfo by name
lookupFunction :: String -> [FunctionInfo] -> Maybe FunctionInfo
lookupFunction funcName = find (\doc -> functionName doc == funcName)

-- | Format function information for display in LSP hover
formatTidalFunction :: FunctionInfo -> Text
formatTidalFunction FunctionInfo{..} = T.pack $
            functionType ++ "\n" ++
            "---" ++ "\n\n" ++
            functionDocumentation ++ "\n\n"

printFunctionInfo :: FunctionInfo -> IO ()
printFunctionInfo FunctionInfo {..} = do
    putStrLn $ functionName ++ " :: " ++ functionType
    putStrLn $ unlines $ map ("  " ++) $ lines functionDocumentation

-- | Test function to find and print documentation for a function
testFindFunction :: String -> IO ()
testFindFunction funcName = do
    result <- findTidalFunction funcName
    case result of
        Just functionInfo -> printFunctionInfo functionInfo
        Nothing -> putStrLn $ "Function '" ++ funcName ++ "' not found !"

-- | Test function to extract and display documentation from a specific file
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
            functionDocumentation ++ "\n\n"
