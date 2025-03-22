{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Char (toLower, toUpper)
import Languages (decodeJsonFile, languages, transformMap)
import Options.Applicative
import Statistics (ExportType (..), countLinesInDirs, formatStatistics)
import Text.Read (readMaybe)

data CliOptions = CliOptions
  { filePaths :: [FilePath],
    byFile :: Bool,
    exportType :: ExportType,
    exclude :: Maybe String
  }
  deriving (Show)

exportTypeParser :: Parser ExportType
exportTypeParser =
  option
    readExportType
    ( long "export"
        <> metavar "EXPORT_TYPE"
        <> help "Specify the export type"
        <> value Tabular
        <> showDefaultWith (const "Tabular")
    )
  where
    readExportType = eitherReader $ \arg ->
      maybe (Left $ "Invalid export type: " ++ arg) Right (readMaybe (toUpper (head arg) : (toLower <$> tail arg)))

excludePathsParser :: Parser (Maybe String)
excludePathsParser =
  optional
    ( strOption
        ( long "exclude"
            <> metavar "PATTERN"
            <> help "File path pattern to exclude"
            <> action "file"
        )
    )

cliOptionsParser :: Parser CliOptions
cliOptionsParser =
  CliOptions
    <$> some (argument str (metavar "PATHS..."))
    <*> switch (long "files" <> short 'f' <> help "Process by file")
    <*> exportTypeParser
    <*> excludePathsParser

main :: IO ()
main = do
  opts <- execParser optsParser
  result <- decodeJsonFile "languages.json"
  case result of
    Left err -> putStrLn $ "Error: " ++ err
    Right l -> do
      let langMap = transformMap (languages l)
      let excludePaths = exclude opts
      let dirs = filePaths opts
      let printByFIle = byFile opts
      let exportType' = exportType opts

      results <- countLinesInDirs langMap excludePaths dirs
      formatStatistics results printByFIle exportType'
  where
    optsParser =
      info
        (cliOptionsParser <**> helper)
        (fullDesc <> progDesc "A command line tool to get file statistics")
