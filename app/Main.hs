module Main (main) where

import Control.Monad (when)
import Data.Function ((&))
import Options.Applicative
import Streamly.Data.Fold (Fold, Tee (..))
import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Data.Stream as Stream
import qualified Streamly.FileSystem.File as File
import Streamly.Unicode.Stream as UnicodeStream
import System.IO

data WCOptions = WCOptions
  { countBytes :: Bool,
    countChars :: Bool,
    countLines :: Bool,
    filePath :: FilePath
  }
  deriving (Show)

wcOptionsParser :: Parser WCOptions
wcOptionsParser =
  WCOptions
    <$> switch (long "bytes" <> short 'c' <> help "Count bytes")
    <*> switch (long "chars" <> short 'm' <> help "Count chars")
    <*> switch (long "lines" <> short 'l' <> help "Count lines")
    <*> argument str (metavar "FILE" <> help "File to process")

countLines'' :: Int -> Char -> Int
countLines'' n ch = if ch == '\n' then n + 1 else n

countLines' :: (Monad m) => Fold m Char Int
countLines' = Fold.foldl' countLines'' 0

countDecoded :: Fold IO Char (Int, Int)
countDecoded = unTee $ (,) <$> Tee Fold.length <*> Tee countLines'

processFile :: WCOptions -> IO ()
processFile opts = do
  let file = filePath opts
  handle <- openFile file ReadMode

  let stream = File.read file

  numbytes <- stream & Stream.fold Fold.length
  (numchars, numlines) <- stream & UnicodeStream.decodeUtf8 & Stream.fold countDecoded

  hClose handle

  when (countLines opts) $ print numlines
  when (countChars opts) $ print numchars
  when (countBytes opts) $ print numbytes

main :: IO ()
main = execParser opts >>= processFile
  where
    opts =
      info
        (wcOptionsParser <**> helper)
        ( fullDesc
            <> progDesc "wc utility implemented in Haskell"
        )
