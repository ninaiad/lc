{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Statistics (countLinesInDirs, formatStatistics) where

import Control.Monad (filterM)
import Data.Aeson (ToJSON (..), defaultOptions, genericToEncoding)
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

aggregateStats :: [FileStats] -> [(String, Int, Int, Int, Int, Int)]
aggregateStats stats =
  let grouped = groupBy (\a b -> languageName a == languageName b) (sortOn languageName stats)
   in map summarize grouped
  where
    summarize :: [FileStats] -> (String, Int, Int, Int, Int, Int)
    summarize files =
      let lang = fromMaybe "Unknown" (languageName (head files))
          fileCount = length files
          totalLines = sum (map lineCount files)
          totalComments = sum (map commentLineCount files)
          totalBlanks = sum (map blankLineCount files)
       in (lang, fileCount, totalLines, totalLines - totalComments - totalBlanks, totalComments, totalBlanks)

computeWidths :: [(String, Int, Int, Int, Int, Int)] -> (Int, Int, Int, Int, Int, Int)
computeWidths rows =
  let langWidth = maximum $ map (length . sel1) rows ++ [8]
      numWidth selector = maximum $ map (length . show . selector) rows ++ [8]
   in ( langWidth,
        numWidth (\(_, f, _, _, _, _) -> f),
        numWidth (\(_, _, l, _, _, _) -> l),
        numWidth (\(_, _, _, c, _, _) -> c),
        numWidth (\(_, _, _, _, com, _) -> com),
        numWidth (\(_, _, _, _, _, b) -> b)
      )

headerFormat :: Int -> Int -> Int -> Int -> Int -> Int -> String
headerFormat langW fileW lineW codeW comW blankW =
  printf
    " %-*s %-*s %-*s %-*s %-*s %-*s\n"
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
    " %-*s %-*d %-*d %-*d %-*d %-*d\n"
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

printStatsTable :: [FileStats] -> IO ()
printStatsTable stats = do
  let aggregated = aggregateStats stats
      (langW, fileW, lineW, codeW, comW, blankW) = computeWidths aggregated
      totalFiles = sum [f | (_, f, _, _, _, _) <- aggregated]
      totalLines = sum [l | (_, _, l, _, _, _) <- aggregated]
      totalCode = sum [c | (_, _, _, c, _, _) <- aggregated]
      totalComments = sum [com | (_, _, _, _, com, _) <- aggregated]
      totalBlanks = sum [b | (_, _, _, _, _, b) <- aggregated]

      separator = replicate (langW + fileW + lineW + codeW + comW + blankW + 7) '─'
      boldSeparator = replicate (langW + fileW + lineW + codeW + comW + blankW + 7) '━'

  putStrLn boldSeparator
  printf (headerFormat langW fileW lineW codeW comW blankW)
  putStrLn separator

  let dataFormat' = dataFormat langW fileW lineW codeW comW blankW

  mapM_
    ( \(lang, files, lines', code, comments, blanks) ->
        printf (dataFormat' lang files lines' code comments blanks)
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

formatStatistics :: [FileStats] -> IO ()
formatStatistics stats = do
  -- let totalLines = foldl' (+) 0 (map lineCount stats)
  -- let totalComments = foldl' (+) 0 (map commentLineCount stats)
  -- let totalBlank = foldl' (+) 0 (map blankLineCount stats)

  printStatsTable stats

-- let boldSeparator = replicate 100 '━'
-- -- let separator = replicate 32 '─'

-- putStrLn boldSeparator

-- PP.printTable stat

-- putStrLn boldSeparator

-- mapM_ (print . Data.Aeson.encode) stats
-- putStrLn boldSeparator
-- mapM_ (print . Data.YAML.encode) [stats]
-- putStrLn boldSeparator