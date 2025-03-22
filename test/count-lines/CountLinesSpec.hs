{-# LANGUAGE OverloadedStrings #-}

import Statistics (countLinesInFile)
import System.Directory (removeFile)
import Test.HUnit (assertEqual)
import Test.Hspec

withTempFile :: String -> (FilePath -> IO a) -> IO a
withTempFile content action = do
  let filePath = "temp.txt"
  writeFile filePath content
  result <- action filePath
  removeFile filePath
  return result

main :: IO ()
main = hspec $ do
  describe "Counts file statistics" $ do
    it "Counts total number of lines, comments, and blanks in a file" $ do
      let content =
            unlines
              [ "-- This is a comment",
                "{- ",
                "This",
                "is",
                "a",
                "multi-line",
                "comment -}",
                "",
                "Not a comment",
                ""
              ]
          commentPrefixes = ["--"]
          multiLineComments = [("{-", "-}")]

      withTempFile content $ \filePath -> do
        (total, comments, blank) <- countLinesInFile filePath commentPrefixes multiLineComments
        assertEqual "total lines" 10 total
        assertEqual "commented lines" 7 comments
        assertEqual "blank lines" 2 blank

    it "Handles files with neither comments nor blank lines" $ do
      let content =
            unlines
              [ "foo",
                "bar"
              ]
          commentPrefixes = ["--"]
          multiLineComments = [("{-", "-}")]

      withTempFile content $ \filePath -> do
        (total, comments, blank) <- countLinesInFile filePath commentPrefixes multiLineComments
        assertEqual "total lines" 2 total
        assertEqual "commented lines" 0 comments
        assertEqual "blank lines" 0 blank

    it "Handles files with no comments or blank lines to search for" $ do
      let content =
            unlines
              [ "foo",
                "bar"
              ]
          commentPrefixes = []
          multiLineComments = []

      withTempFile content $ \filePath -> do
        (total, comments, blank) <- countLinesInFile filePath commentPrefixes multiLineComments
        assertEqual "total lines" 2 total
        assertEqual "commented lines" 0 comments
        assertEqual "blank lines" 0 blank

    it "Correctly counts comments of different style" $ do
      let content =
            unlines
              [ "-- This is a comment",
                "// Another comment",
                "{- Multi-line",
                "   comment -}",
                "/* blah-blah-blah */",
                "/*",
                "comment",
                "*/"
              ]
          commentPrefixes = ["--", "//"]
          multiLineComments = [("{-", "-}"), ("/*", "*/")]

      withTempFile content $ \filePath -> do
        (total, comments, blank) <- countLinesInFile filePath commentPrefixes multiLineComments
        assertEqual "total lines" 8 total
        assertEqual "commented lines" 8 comments
        assertEqual "blank lines" 0 blank

    it "Handles files with blank lines only" $ do
      let content =
            unlines
              [ "",
                "",
                ""
              ]
          commentPrefixes = ["--"]
          multiLineComments = [("{-", "-}")]

      withTempFile content $ \filePath -> do
        (total, comments, blank) <- countLinesInFile filePath commentPrefixes multiLineComments
        assertEqual "total lines" 3 total
        assertEqual "commented lines" 0 comments
        assertEqual "blank lines" 3 blank

    it "Handles empty files" $ do
      let content = ""
          commentPrefixes = ["--"]
          multiLineComments = [("{-", "-}")]

      withTempFile content $ \filePath -> do
        (total, comments, blank) <- countLinesInFile filePath commentPrefixes multiLineComments
        assertEqual "total lines" 0 total
        assertEqual "commented lines" 0 comments
        assertEqual "blank lines" 0 blank
