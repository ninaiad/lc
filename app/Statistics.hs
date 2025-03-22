{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Statistics (countLinesInDirs, formatStatistics, ExportType (..)) where

import Control.Monad (filterM, when)
import Data.Aeson (ToJSON (..), defaultOptions, encode, genericToEncoding)
import qualified Data.ByteString.Lazy as B
import Data.Data (Data)
import Data.List (groupBy, sortOn)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Tuple.Select
import Data.YAML
import GHC.Generics (Generic)
import Languages (LanguageInfo (..))
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath (takeExtension, (</>))
import Text.Printf (printf)
import Text.Regex.TDFA ((=~))

data ExportType = Tabular | Json | Yaml deriving (Show, Read, Eq)

data FileStats = FileStats
  { filePath :: FilePath,
    lineCount :: Int,
    commentLineCount :: Int,
    blankLineCount :: Int,
    languageName :: Maybe String
  }
  deriving (Data, Show, Generic)

instance ToJSON FileStats where
  toEncoding = genericToEncoding defaultOptions

instance ToYAML FileStats where
  toYAML (FileStats f l c b ln) =
    mapping
      [ "filePath" .= T.pack f,
        "lineCount" .= l,
        "commentLineCount" .= c,
        "blankLineCount" .= b,
        "languageName"
          .= ( \case
                 Just name -> T.pack name
                 Nothing -> "Unknown"
             )
            ln
      ]

aggregateStats :: [FileStats] -> [(String, Int, Int, Int, Int, Int, [(FilePath, Int, Int, Int, Int)])]
aggregateStats stats =
  let grouped = groupBy (\a b -> languageName a == languageName b) $ sortOn languageName stats
   in map
        ( \group ->
            let lang = fromMaybe "Unknown" (languageName (head group))
                numFiles = length group
                totalLines = sum (map lineCount group)
                totalCode = sum (map (\fs -> lineCount fs - commentLineCount fs - blankLineCount fs) group)
                totalComments = sum (map commentLineCount group)
                totalBlanks = sum (map blankLineCount group)
                fileDetails = [(filePath fs, lineCount fs, lineCount fs - commentLineCount fs - blankLineCount fs, commentLineCount fs, blankLineCount fs) | fs <- group]
             in (lang, numFiles, totalLines, totalCode, totalComments, totalBlanks, fileDetails)
        )
        grouped

computeWidths :: [(String, Int, Int, Int, Int, Int, [(FilePath, Int, Int, Int, Int)])] -> (Int, Int, Int, Int, Int, Int)
computeWidths rows =
  let langWBase = maximum $ map (length . sel1) rows ++ [8] -- At least "Language"
      fileW = maximum $ map (length . show . sel2) rows ++ [5]
      lineW = maximum $ map (length . show . sel3) rows ++ [5]
      codeW = maximum $ map (length . show . sel4) rows ++ [4]
      comW = maximum $ map (length . show . sel5) rows ++ [8]
      blankW = maximum $ map (length . show . sel6) rows ++ [6]

      longestFilePath = maximum $ 0 : concatMap (\(_, _, _, _, _, _, files) -> map (length . sel1) files) rows
      totalMinWidth = langWBase + fileW
      extraNeeded = max 0 (longestFilePath - totalMinWidth)
      langW = langWBase + extraNeeded
   in (langW, fileW, lineW, codeW, comW, blankW)

headerFormat :: Int -> Int -> Int -> Int -> Int -> Int -> String
headerFormat langW fileW lineW codeW comW blankW =
  printf
    " %-*s %*s %*s %*s %*s %*s\n"
    langW
    ("Language" :: String)
    fileW
    ("Files" :: String)
    lineW
    ("Lines" :: String)
    codeW
    ("Code" :: String)
    comW
    ("Comments" :: String)
    blankW
    ("Blanks" :: String)

dataFormat :: Int -> Int -> Int -> Int -> Int -> Int -> String -> Int -> Int -> Int -> Int -> Int -> String
dataFormat langW fileW lineW codeW comW blankW lang files lines' code comments =
  printf
    " %-*s %*d %*d %*d %*d %*d\n"
    langW
    lang
    fileW
    files
    lineW
    lines'
    codeW
    code
    comW
    comments
    blankW

fileFormat :: Int -> Int -> Int -> Int -> Int -> String -> Int -> Int -> Int -> Int -> String
fileFormat pathW lineW codeW comW blankW filepath lines' code comments =
  printf
    " %-*s  %*d %*d %*d %*d\n"
    pathW
    filepath
    lineW
    lines'
    codeW
    code
    comW
    comments
    blankW

printStatsTable :: Bool -> [FileStats] -> IO ()
printStatsTable byFile stats = do
  let aggregated = aggregateStats stats
      (langW, fileW, lineW, codeW, comW, blankW) = computeWidths aggregated
      totalFiles = sum [f | (_, f, _, _, _, _, _) <- aggregated]
      totalLines = sum [l | (_, _, l, _, _, _, _) <- aggregated]
      totalCode = sum [c | (_, _, _, c, _, _, _) <- aggregated]
      totalComments = sum [com | (_, _, _, _, com, _, _) <- aggregated]
      totalBlanks = sum [b | (_, _, _, _, _, b, _) <- aggregated]

      separator = replicate (langW + fileW + lineW + codeW + comW + blankW + 7) '─'
      boldSeparator = replicate (langW + fileW + lineW + codeW + comW + blankW + 7) '━'

  putStrLn boldSeparator
  printf (headerFormat langW fileW lineW codeW comW blankW)

  let dataFormat' = dataFormat langW fileW lineW codeW comW blankW
  let fileFormat' = fileFormat (langW + fileW) lineW codeW comW blankW

  mapM_
    ( \(lang, files, lines', code, comments, blanks, fileStats) -> do
        putStrLn separator
        printf (dataFormat' lang files lines' code comments blanks)

        when byFile $ do
          putStrLn separator
          mapM_
            ( \(fp, fLines, fCode, fComments, fBlanks) ->
                printf (fileFormat' fp fLines fCode fComments fBlanks)
            )
            fileStats
    )
    aggregated

  putStrLn separator
  printf (dataFormat' "Total" totalFiles totalLines totalCode totalComments totalBlanks)
  putStrLn boldSeparator

countLines :: FilePath -> [String] -> IO (Int, Int, Int)
countLines file commentPrefixes = do
  content <- TIO.readFile file
  let linesList = T.lines content
      total = length linesList
      comments = length (filter (\line -> any ((`T.isPrefixOf` line) . T.pack) commentPrefixes) linesList)
      blank = length (filter T.null linesList)
  return (total, comments, blank)

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
  let ext = drop 1 (takeExtension f)
      langInfo = M.lookup ext langMap
      commentPrefixes = maybe [] lineComment' langInfo
      langName = fmap name' langInfo
  (c, p, b) <- countLines f commentPrefixes
  return (FileStats f c p b langName)

countLinesInDirs :: M.Map String LanguageInfo -> Maybe String -> [FilePath] -> IO [FileStats]
countLinesInDirs langMap exclude' dirs = do
  allFiles <- concat <$> mapM (getFilesRecursively exclude') dirs
  mapM (processFile langMap) allFiles

formatStatistics :: [FileStats] -> Bool -> ExportType -> IO ()
formatStatistics stats printByFile exportType = do
  case exportType of
    Tabular -> printStatsTable printByFile stats
    Json -> B.putStr (Data.Aeson.encode stats)
    Yaml -> B.putStr (Data.YAML.encode stats)
