{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
-----------------------------------------------------------------------------
--
-- Module      :  LamdaHofUtils
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module LamdahofUtils (
    replaceOne,
    Defaultable (..),
    LamdahofUtils.elem,
    hasA,

    exeParser,
    parserResultUnsafe,
    testParser,
    testParserEqualsExpected,
    testParserResult,
    fileToText,
    testDataDir,
    testDataText,
    tempDir,
    testParserE,
    runTestDataFileTextIntegrationTest,
    testDataFile,
    safeLast,
    safeFirst,
    unlinesTrue,
    linesTrue,
    appendDelim,

    fullPath,
    findFilesMatchingRecursive,
    findFilesRecursive,
    findFilesNonRecursive,
    listDirectories,
    listDirectoriesRecursive,
    matchesPattern,
    isDirectoryFindClause,
    isRegularFile,
    safeLoader,

    consoleRed,
    consoleGreen

) where

import           Control.Exception.Base

import           Control.Monad
import qualified Data.ByteString             as BS
import           Data.Either
import           Data.Either.Combinators     hiding (isRight)
import           Data.List
import           Data.Maybe
import           Data.Monoid
import           Data.Text                   as Text
import qualified Data.Text.Encoding          as TextEncoding
import           Debug.Trace
import qualified Foundation.Extended         as F
import           Paths_Parser
import           System.Console.ANSI
import           System.Directory
import           System.FilePath
import           System.FilePath.Find
import           System.FilePath.GlobPattern
import           System.IO                   as IO
import           System.IO.Error
import           Test.Extended
import           Text.Megaparsec

(?) = (F.?)

replaceOne :: Text -> Text -> Text -> Text
replaceOne needle replaceTxt haystack
                                    | Text.null back = haystack    -- pattern doesn't occur
                                    | otherwise = Text.concat [front, replaceTxt, Text.drop (Text.length needle) back]
                                  where
                                    (front, back) = breakOn needle haystack


fullPath :: FilePath -> FilePath -> IO FilePath
fullPath absBase relativePath = canonicalizePath $ absBase </> relativePath

consoleGreen ::  IO a -> IO a
consoleGreen ioAction = do
                           setSGR [SetColor Foreground Vivid Green]
                           exResetConsole ioAction

consoleRed ::  IO a -> IO a
consoleRed ioAction = do
                        setSGR [SetColor Foreground Vivid Red]
                        setSGR [SetColor Background Vivid White]
                        exResetConsole ioAction

exResetConsole ::  IO a -> IO a
exResetConsole ioAction = do
                            a <- ioAction
                            setSGR [Reset]
                            pure a


appendDelim :: Text -> Text -> Text -> Text
appendDelim base delim additional = (Text.null base || Text.null additional) ?
                                                 (base <> additional) $
                                                 base <> delim <> additional


isFileTypeOf :: FileType -> FindClause Bool
isFileTypeOf = (fileType ==?)

isRegularFile :: FindClause Bool
isRegularFile = isFileTypeOf RegularFile

isDirectoryFindClause :: FindClause Bool
isDirectoryFindClause = isFileTypeOf Directory

matchesPattern :: GlobPattern -> FindClause Bool
matchesPattern pat = fileName ~~? pat

listDirectories :: FilePath -> IO [FilePath]
listDirectories dir = findFilesRecursiveWithDepth dir (depth <? 1) isDirectoryFindClause

listDirectoriesRecursive :: FilePath -> IO [FilePath]
listDirectoriesRecursive dir = findFilesNonRecursive dir isDirectoryFindClause

findFilesRecursiveWithDepth :: FilePath -> FindClause Bool -> FindClause Bool -> IO [FilePath]
findFilesRecursiveWithDepth dir depthFinder finder = System.FilePath.Find.find depthFinder finder dir

findFilesNonRecursive :: FilePath -> FindClause Bool -> IO [FilePath]
findFilesNonRecursive dir = findFilesRecursiveWithDepth dir (depth <? 1)

findFilesRecursive :: FilePath -> FindClause Bool -> IO [FilePath]
findFilesRecursive dir = findFilesRecursiveWithDepth dir always

findFilesMatchingRecursive :: FilePath -> GlobPattern -> IO [FilePath]
findFilesMatchingRecursive dir pat = findFilesRecursive dir (isRegularFile &&? matchesPattern pat)

linesTrue :: Text -> [Text]
linesTrue txt = (not (Text.null txt) && Text.last txt == '\n') ? Text.lines (snoc txt '\n') $ Text.lines txt

unlinesTrue :: [Text] -> Text
unlinesTrue []  = ""
unlinesTrue txt = Text.init $ Text.unlines txt

safeLast :: [a] -> Maybe a
safeLast = safeOp Data.List.last

safeFirst :: [a] -> Maybe a
safeFirst = safeOp Data.List.head

safeOp :: ([a] -> a) -> [a] -> Maybe a
safeOp unSafeFunc list = Data.List.null list ? Nothing $ Just $ unSafeFunc list



runTestDataFileTextIntegrationTest  :: F.RelFile -> (Text -> Assertion) -> Assertion
runTestDataFileTextIntegrationTest filePath textAssertion = do
                                                              ethTxt <- testDataText filePath
                                                              F.eitherf ethTxt
                                                                (\err -> chkFail ("problem loading " <> F.toFilePath filePath <> ": " <> show err))
                                                                textAssertion

notExistError :: FilePath -> Either IOError FilePath
notExistError message = Left (mkIOError doesNotExistErrorType message Nothing Nothing)

directoryElement :: Either IOError FilePath -> FilePath -> (FilePath -> IO Bool) -> IO (Either IOError FilePath)
directoryElement dirEith childName elementPredicate =
                          isRight dirEith ?
                                            do
                                              let dir = def dirEith ""
                                                  fullName = combine dir childName
                                              exists <- elementPredicate fullName
                                              exists ?
                                                pure (Right fullName) $
                                                pure (notExistError fullName)
                                            $
                                             pure dirEith

subFile :: Either IOError FilePath -> FilePath -> IO (Either IOError FilePath)
subFile dirEith childDir = directoryElement dirEith childDir doesFileExist

binDir = F.parseAbsDir =<< getBinDir

fromBin = F.subDirFromBaseDir binDir

tempDir :: IO (Either IOError F.AbsDir)
tempDir = fromBin [F.reldir|Temp|]

testDataDir ::  IO (Either IOError F.AbsDir)
testDataDir = fromBin [F.reldir|TestData|]

testDataFile :: F.RelFile -> IO (Either IOError F.AbsFile)
testDataFile relFile = do
                        tdd <- testDataDir
                        pure $ (F.</> relFile) <$> tdd

testDataText :: F.RelFile -> IO (Either F.StrictReadError Text)
testDataText relFile = do
                        df <- testDataFile relFile
                        F.eitherf df
                          (pure . Left . F.IOFailure)
                          fileText

fileText :: F.AbsFile -> IO (Either F.StrictReadError Text)
fileText file = do
                 eStr <- F.readFileUTF8 file
                 pure $ F.toText <$> eStr

myReadFile :: FilePath -> IO (Either IOError Text)
myReadFile fullFilePath = do
                            h    <- openFile fullFilePath IO.ReadMode
                            bs   <- BS.hGetContents h
                            pure $ pure $ TextEncoding.decodeUtf8 bs

-- found to truncate files under some circumstances hence using myreadfile instead
-- C:\Automation\QuickStep\Parser\testData\CCB_Duplicate_Invalid_Summons_Judgements_Bankruptcies - Broken.rslt
-- fileToTextBroken :: FilePath -> IO (Either IOError Text)
-- fileToTextBroken fullPath = tryJust (getError isDoesNotExistError) (Data.Text.IO.readFile fullPath)

fileToText :: FilePath -> IO (Either IOError Text)
fileToText = myReadFile

-- getError ::  (IOError -> Bool) -> IOError -> Maybe IOError
-- getError predicate err = predicate err ? Just err $ Nothing

hasSubDir :: FilePath -> FilePath -> IO Bool
hasSubDir parentDir sub = doesDirectoryExist $ combine parentDir sub

-- textIs  ::  (FilePath -> IO Bool) -> Text -> IO Bool
-- textIs predic = predic.unpack

-- isFile :: Text -> IO Bool
-- isFile = textIs doesFileExist

-- isDirectory :: Text -> IO Bool
-- isDirectory = textIs doesDirectoryExist

safeLoader :: (Exception e) => Text -> (Text -> IO a) ->  IO (Either e a)
safeLoader filePth fileLoader =  Control.Exception.Base.try (fileLoader filePth)

exeParser ::  (Stream s, Eq r, Show r) => Parsec e s r -> s ->  Either (ParseError (Token s) e) r
exeParser parser = runParser parser "Unit Test"


-- only for setting up test data parses successfully or blows up with exception
parserResultUnsafe :: (Stream i, Eq r, Show r) => Parsec e i r -> i -> r
parserResultUnsafe parser input = fromRight' $ exeParser parser input

testParserResult :: (Stream parseInput, ShowToken (Token parseInput), Eq parseResult, Show parseResult, Eq testTarget, Show testTarget, ShowErrorComponent erroComp) =>
                 Bool -> -- wantLogging
                 (parseResult -> testTarget) -> -- converter
                 (testTarget -> Assertion) ->  -- test
                 Parsec erroComp parseInput parseResult ->  -- parser
                 parseInput -> -- input
                 Assertion
testParserResult wantLogging converter test parser input = eitherToAssertion  (exeParser parser input) converter test
                                                            where
                                                             eitherToAssertion ::  (Eq testTarget, Ord token, Show testTarget, ShowErrorComponent err, ShowToken token, Show parseResult) => Either (ParseError token err) parseResult -> (parseResult -> testTarget) -> (testTarget -> Assertion)  -> Assertion
                                                             eitherToAssertion eth convertr tst = case eth of
                                                                                                      Right actual  -> do
                                                                                                                          let testVal = convertr actual
                                                                                                                          when wantLogging $
                                                                                                                              do
                                                                                                                                 IO.print actual
                                                                                                                                 Prelude.putStrLn $ "converted actual is: " ++ show testVal
                                                                                                                          tst testVal
                                                                                                      Left err        -> do
                                                                                                                          when wantLogging $
                                                                                                                             Prelude.putStrLn $ parseErrorPretty err
                                                                                                                          assertFailure $ parseErrorPretty err

testParserEqualsExpected :: (Stream parseInput, ShowToken (Token parseInput), Eq parseResult, Show parseResult, Eq expectedResult, Show expectedResult, ShowErrorComponent erroComp)
                         => Bool -> (parseResult -> expectedResult) -> Parsec erroComp parseInput parseResult -> parseInput -> expectedResult -> Assertion
testParserEqualsExpected wantLogging converter parser input expected = testParserResult wantLogging converter (chkEq expected) parser input

testParser :: (Eq a, (Stream s), Ord (Token s), Show a, ShowToken (Token s), ShowErrorComponent e) => Bool -> Parsec e s a -> s -> a -> Assertion
testParser wantLogging = testParserEqualsExpected wantLogging id

testParserE ::  (Ord (Token s), Stream s, Show t, Eq t, Show e, Show (Token s), ShowToken (Token s), ShowErrorComponent e) => Bool -> Parsec e s t ->  Text -> s -> Assertion
testParserE wantLogging parser errorFrag input = chkLeftContains' parseErrorPretty errorFrag (exeParser parser input)

hasA :: (Eq a) => [a] -> a -> Bool
hasA = flip Data.List.elem

class Defaultable a where
  def :: a b -> b -> b

instance Defaultable (Either m)  where
   def e d = either (const d) id e

instance Defaultable Maybe where
   def m d = fromMaybe d m


-- http://lukasepple.de/posts/2015-01-06-elem-for-text.html
elem :: Char -> Text -> Bool
elem c str = isJust (Text.findIndex (== c) str)
