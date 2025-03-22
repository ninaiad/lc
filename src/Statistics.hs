{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Statistics (countLines, countLinesInFile, doPrint, ExportType (..)) where

import Conduit
import Control.Monad (filterM, when)
import Data.Aeson (ToJSON (..), defaultOptions, encode, genericToEncoding)
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as BSL
import Data.Data (Data)
import Data.List (find, groupBy, sortOn)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Tuple.Select
import Data.YAML
import GHC.Generics (Generic)
import Languages (LanguageInfo (..))
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath (takeExtension, (</>))
import System.IO (IOMode (ReadMode), withFile)
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

--------------------------------------------------------------------------------------

{- Computing file statistics -}

aggregate :: [FileStats] -> [(String, Int, Int, Int, Int, Int, [(FilePath, Int, Int, Int, Int)])]
aggregate stats =
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

countLinesInFile :: FilePath -> [String] -> [(String, String)] -> IO (Int, Int, Int)
countLinesInFile file commentPrefixes multiLineComments =
  withFile file ReadMode $ \handle -> do
    (total, comments, blank, _) <-
      runConduit $
        sourceHandle handle
          .| mapC C8.unpack
          .| linesUnboundedC
          .| mapC C8.pack
          .| foldlC processLine (0, 0, 0, Nothing)
    return (total, comments, blank)
  where
    processLine :: (Int, Int, Int, Maybe (Int, String)) -> C8.ByteString -> (Int, Int, Int, Maybe (Int, String))
    processLine (total, comments, blank, multiState) line =
      let total' = total + 1
          blanks' = if C8.null line then blank + 1 else blank
          (comments', multiState') = updateCommentCounts line commentPrefixes multiLineComments comments multiState
       in (total', comments', blanks', multiState')

updateCommentCounts :: C8.ByteString -> [String] -> [(String, String)] -> Int -> Maybe (Int, String) -> (Int, Maybe (Int, String))
updateCommentCounts line commentPrefixes multiLineComments comments multiState =
  case multiState of
    Just (count, endMarker) ->
      if C8.pack endMarker `C8.isInfixOf` line
        then (comments + count + 1, Nothing)
        else (comments, Just (count + 1, endMarker))
    Nothing ->
      case find (\(openMarker, _) -> C8.pack openMarker `C8.isPrefixOf` line) multiLineComments of
        Just (_, closeMarker) -> (comments, Just (1, closeMarker))
        Nothing ->
          if any ((`C8.isPrefixOf` line) . C8.pack) commentPrefixes
            then (comments + 1, Nothing)
            else (comments, Nothing)

getFilesRecursively :: Maybe String -> Bool -> FilePath -> IO [FilePath]
getFilesRecursively excludePattern includeHidden inDir = do
  contents <- listDirectory inDir
  let visibleContents = if includeHidden then contents else filter (not . isHidden) contents
      paths = map (inDir </>) visibleContents
  files <- filterM doesFileExist paths
  dirs <- filterM doesDirectoryExist paths
  let filteredFiles = case excludePattern of
        Just pat -> filter (not . (=~ pat)) files
        Nothing -> files
  nestedFiles <- fmap concat (mapM (getFilesRecursively excludePattern includeHidden) dirs)
  return $ filteredFiles ++ nestedFiles
  where
    isHidden ('.' : _) = True
    isHidden _ = False

processFile :: M.Map String LanguageInfo -> FilePath -> IO FileStats
processFile langMap f = do
  let ext = drop 1 (takeExtension f)
      langInfo = M.lookup ext langMap
      commentPrefixes = maybe [] lineComment' langInfo
      multiLineComment' = maybe [] multiLineComment langInfo
      langName = fmap name' langInfo
  (c, p, b) <- countLinesInFile f commentPrefixes multiLineComment'
  return (FileStats f c p b langName)

countLines :: [FilePath] -> M.Map String LanguageInfo -> Maybe String -> Bool -> IO [FileStats]
countLines dirs langMap exclude' includeHidden = do
  allFiles <- concat <$> mapM (getFilesRecursively exclude' includeHidden) dirs
  mapM (processFile langMap) allFiles

--------------------------------------------------------------------------------------

{- Formatting, printing -}

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

computeWidths :: [(String, Int, Int, Int, Int, Int, [(FilePath, Int, Int, Int, Int)])] -> Bool -> (Int, Int, Int, Int, Int, Int)
computeWidths rows byFile =
  let langWBase = maximum $ map (length . sel1) rows ++ [8] -- At least "Language"
      fileW = maximum $ map (length . show . sel2) rows ++ [5]
      lineW = maximum $ map (length . show . sel3) rows ++ [5]
      codeW = maximum $ map (length . show . sel4) rows ++ [4]
      comW = maximum $ map (length . show . sel5) rows ++ [8]
      blankW = maximum $ map (length . show . sel6) rows ++ [6]

      langW =
        if byFile
          then
            let longestFilePath = maximum $ 0 : concatMap (\(_, _, _, _, _, _, files) -> map (length . sel1) files) rows
                totalMinWidth = langWBase + fileW
                extraNeeded = max 0 (longestFilePath - totalMinWidth)
             in langWBase + extraNeeded
          else
            langWBase
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

printTable :: [FileStats] -> Bool -> IO ()
printTable statistics byFile = do
  let aggregated = aggregate statistics
      (langW, fileW, lineW, codeW, comW, blankW) = computeWidths aggregated byFile
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
      fileFormat' = fileFormat (langW + fileW) lineW codeW comW blankW

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

doPrint :: [FileStats] -> Bool -> ExportType -> IO ()
doPrint statistics byFile exportType = do
  case exportType of
    Tabular -> printTable statistics byFile
    Json -> BSL.putStr (Data.Aeson.encode statistics)
    Yaml -> BSL.putStr (Data.YAML.encode statistics)
