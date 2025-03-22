{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Map as M
import Languages
import System.Directory (removeFile)
import Test.HUnit (assertEqual, assertFailure)
import Test.Hspec

withTempFile :: String -> (FilePath -> IO a) -> IO a
withTempFile content action = do
  let filePath = "languages_test.json"
  writeFile filePath content
  result <- action filePath
  removeFile filePath
  return result

main :: IO ()
main = hspec $ do
  describe "Parse languages.json" $ do
    it "Parses file correctly" $ do
      let content =
            "{\"languages\":{\
            \\"Abap\":{\"name\":\"ABAP\",\"lineComment\":[\"*\",\"\\\\\\\"\"],\"extensions\":[\"abap\"]},\
            \\"Ada\":{\"lineComment\":[\"--\"],\"extensions\":[\"ada\",\"adb\",\"ads\",\"pad\"]},\
            \\"Haskell\":{\"nested\":true,\"lineComment\":[\"--\"],\"multiLineComments\":[[\"{-\",\"-}\"]],\"extensions\":[\"hs\"]},\
            \\"Markdown\":{\"literate\":true,\"important_syntax\":[\"```\"],\"extensions\":[\"md\",\"markdown\"]}\
            \}}"

      let expectedDecoded =
            Languages
              { languages =
                  M.fromList
                    [ -- a specified name, multiple lineComment
                      ("Abap", Language {name = Just "ABAP", lineComment = ["*", "\\\""], multiLineComments = [], extensions = ["abap"]}),
                      -- many extensions
                      ("Ada", Language {name = Nothing, lineComment = ["--"], multiLineComments = [], extensions = ["ada", "adb", "ads", "pad"]}),
                      -- multiLineComments
                      ("Haskell", Language {name = Nothing, lineComment = ["--"], multiLineComments = [("{-", "-}")], extensions = ["hs"]}),
                      -- no comments
                      ("Markdown", Language {name = Nothing, lineComment = [], multiLineComments = [], extensions = ["md", "markdown"]})
                    ]
              }

      let expectedTransformed =
            M.fromList
              [ ("abap", LanguageInfo {name' = "ABAP", lineComment' = ["*", "\\\""], multiLineComment = []}),
                ("ada", LanguageInfo {name' = "Ada", lineComment' = ["--"], multiLineComment = []}),
                ("adb", LanguageInfo {name' = "Ada", lineComment' = ["--"], multiLineComment = []}),
                ("ads", LanguageInfo {name' = "Ada", lineComment' = ["--"], multiLineComment = []}),
                ("pad", LanguageInfo {name' = "Ada", lineComment' = ["--"], multiLineComment = []}),
                ("hs", LanguageInfo {name' = "Haskell", lineComment' = ["--"], multiLineComment = [("{-", "-}")]}),
                ("md", LanguageInfo {name' = "Markdown", lineComment' = [], multiLineComment = []}),
                ("markdown", LanguageInfo {name' = "Markdown", lineComment' = [], multiLineComment = []})
              ]

      withTempFile content $ \filePath -> do
        decoded <- decodeJsonFile filePath
        case decoded of
          Left err -> assertFailure err
          Right languages' -> do
            assertEqual "Decoded data" expectedDecoded languages'

            let langMap = transformMap (languages languages')
            assertEqual "Transformed into LanguageInfo" expectedTransformed langMap
