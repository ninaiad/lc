{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (filterM)
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Json (LanguageInfo (..), decodeJsonFile, languages, transformMap)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.Environment (getArgs)
import System.FilePath (takeExtension, (</>))
import Text.Regex.TDFA ((=~))

data FileStats = FileStats
  { filePath :: FilePath,
    lineCount :: Int,
    commentLineCount :: Int,
    blankLineCount :: Int,
    languageName :: Maybe String
  }
  deriving (Show)

countLines :: FilePath -> [String] -> IO (Int, Int, Int)
countLines file commentPrefixes = do
  content <- TIO.readFile file
  let linesList = T.lines content
      total = length linesList
      commented = length (filter (\line -> any ((`T.isPrefixOf` line) . T.pack) commentPrefixes) linesList)
      blank = length (filter T.null linesList)
  return (total, commented, blank)

getFilesRecursively :: Maybe String -> FilePath -> IO [FilePath]
getFilesRecursively excludePattern dir = do
  contents <- listDirectory dir
  let paths = map (dir </>) contents
  files <- filterM doesFileExist paths
  dirs <- filterM doesDirectoryExist paths
  let filteredFiles = case excludePattern of
        Just pat -> filter (not . (=~ pat)) files
        Nothing -> files
  nestedFiles <- fmap concat (mapM (getFilesRecursively excludePattern) dirs)
  return $ filteredFiles ++ nestedFiles

processFile :: M.Map String LanguageInfo -> FilePath -> IO FileStats
processFile langMap f = do
  let ext = drop 1 (takeExtension f) -- Remove leading dot
      langInfo = M.lookup ext langMap
      commentPrefixes = maybe [] lineComment' langInfo
      langName = fmap name' langInfo
  (c, p, b) <- countLines f commentPrefixes
  return (FileStats f c p b langName)

countLinesInDirs :: M.Map String LanguageInfo -> Maybe String -> [FilePath] -> IO ()
countLinesInDirs langMap exclude dirs = do
  allFiles <- concat <$> mapM (getFilesRecursively exclude) dirs
  results <- mapM (processFile langMap) allFiles
  mapM_ (\(FileStats f c p b lang) -> putStrLn $ f ++ ": " ++ show c ++ " lines, " ++ show p ++ " comments, " ++ show b ++ " blanks, Language: " ++ show lang) results


main :: IO ()
main = do
  args <- getArgs
  result <- decodeJsonFile "languages.json"
  case result of
    Left err -> putStrLn $ "Error: " ++ err
    Right l -> do
      let langMap = transformMap (languages l)
      let (exclude, remainingArgs) = case args of
            ("--exclude" : pattern : rest) -> (Just pattern, rest)
            _ -> (Nothing, args)
      case remainingArgs of
        dirs | not (null dirs) -> countLinesInDirs langMap exclude dirs
        _ -> putStrLn "Usage: lc <directory1> <directory2>"