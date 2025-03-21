{-# LANGUAGE DeriveGeneric #-}

module Languages (LanguageInfo (..), languages, decodeJsonFile, transformMap) where

import Data.Aeson
  ( FromJSON (parseJSON),
    eitherDecode,
    withObject,
    (.!=),
    (.:),
    (.:?),
  )
import Data.Aeson.Key (fromString)
import qualified Data.ByteString.Lazy as B
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import GHC.Generics (Generic)

data Language = Language
  { name :: Maybe String,
    lineComment :: [String],
    extensions :: [String]
  }
  deriving (Show, Generic)

data LanguageInfo = LanguageInfo
  { name' :: String,
    lineComment' :: [String]
  }
  deriving (Show)

instance FromJSON Language where
  parseJSON = withObject "Language" $ \v ->
    Language
      <$> v .:? fromString "name"
      <*> (v .:? fromString "lineComment" .!= [])
      <*> v .: fromString "extensions"

newtype Languages = Languages
  { languages :: M.Map String Language
  }
  deriving (Show, Generic)

instance FromJSON Languages where
  parseJSON = withObject "Languages" $ \v ->
    Languages
      <$> v .: fromString "languages"

decodeJsonFile :: FilePath -> IO (Either String Languages)
decodeJsonFile path = do
  jsonData <- B.readFile path
  return $ eitherDecode jsonData

transformMap :: M.Map String Language -> M.Map String LanguageInfo
transformMap langMap = M.fromList $ concatMap extractEntries (M.toList langMap)
  where
    extractEntries :: (String, Language) -> [(String, LanguageInfo)]
    extractEntries (key, lang) =
      let nameValue = fromMaybe key (name lang)
       in [(ext, LanguageInfo nameValue (lineComment lang)) | ext <- extensions lang]
