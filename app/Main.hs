{-# LANGUAGE OverloadedStrings #-}

module Main where

import Languages (decodeJsonFile, languages, transformMap)
import Options.Applicative
import Statistics (countLinesInDirs)
import Text.Read (readMaybe)

data ExportType = JSON | CBOR deriving (Show, Read, Eq)

data CliOptions = CliOptions
  { byFile :: Bool,
    exportType :: Maybe ExportType,
    filePaths :: [FilePath],
    exclude :: Maybe String
  }
  deriving (Show)

exportTypeParser :: Parser (Maybe ExportType)
exportTypeParser =
  optional $
    option
      readExportType
      ( long "export"
          <> short 'e'
          <> metavar "FORMAT"
          <> help "Export format (JSON or CBOR)"
      )
  where
    readExportType = eitherReader $ \arg ->
      maybe (Left $ "Invalid export type: " ++ arg) Right (readMaybe arg)

excludePathsParser :: Parser (Maybe String)
excludePathsParser =
  optional
    ( strOption
        ( long "exclude"
            <> metavar "FILE"
            <> help "File path to exclude"
            <> action "file"
        )
    )

cliOptionsParser :: Parser CliOptions
cliOptionsParser =
  CliOptions
    <$> switch
      ( long "files"
          <> short 'f'
          <> help "Process by file"
      )
    <*> exportTypeParser
    <*> some (argument str (metavar "PATHS..."))
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
      countLinesInDirs langMap excludePaths dirs
  where
    optsParser =
      info
        (cliOptionsParser <**> helper)
        (fullDesc <> progDesc "A command line tool to get file statistics")
