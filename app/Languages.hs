{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Languages (LanguageInfo (..), languages, decodeJsonFile, transformMap) where

import Data.Aeson
  ( FromJSON (parseJSON),
    eitherDecode,
    withObject,
    (.!=),
    (.:),
    (.:?),
  )
import qualified Data.ByteString.Lazy as B
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import GHC.Generics (Generic)

data Language = Language
  { name :: Maybe String,
    lineComment :: [String],
    multiLineComments :: [(String, String)],
    extensions :: [String]
  }
  deriving (Show, Generic)

data LanguageInfo = LanguageInfo
  { name' :: String,
    lineComment' :: [String],
    multiLineComment :: [(String, String)]
  }
  deriving (Show)

instance FromJSON Language where
  parseJSON = withObject "Language" $ \v ->
    Language
      <$> v .:? "name"
      <*> (v .:? "lineComment" .!= [])
      <*> (v .:? "multiLineComments" .!= [])
      <*> v .: "extensions"

newtype Languages = Languages
  { languages :: M.Map String Language
  }
  deriving (Show, Generic)

instance FromJSON Languages where
  parseJSON = withObject "Languages" $ \v ->
    Languages
      <$> v .: "languages"

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
       in [(ext, LanguageInfo nameValue (lineComment lang) (multiLineComments lang)) | ext <- extensions lang]
