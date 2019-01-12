{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}


module ResultParserTestHelpers where

import           Control.Monad
import           Control.Monad.State
import           Data.Either
import           Data.Maybe
import           Data.Monoid
import           Data.Text
import qualified Foundation.Extended       as F
import           LamdahofUtils
import           Paths_Parser
import           ResultInterpreter
import           ResultParser
import           ResultParserShared
import           SubParsers
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.HUnit.Extended
import           Test.Tasty.QuickCheck
import           Test.Tasty.TH
import           Text.Megaparsec
import           Text.Megaparsec.Error



runCheckInterpreter :: (FullInfo -> Assertion) -> Text -> Assertion
runCheckInterpreter infoTest input =
                                    let
                                      parseResult = exeParser rawTestCases input
                                    in
                                      checkParseResult infoTest $ testStats <$> parseResult


runCheckResultOnTestDataFile:: (Eq a, Show a) => (a -> Assertion) -> ResultParser a -> F.RelFile -> Assertion
runCheckResultOnTestDataFile answerTest psr fileName = do
                                                        txtE <- testDataText fileName
                                                        let txt = fromRight "Failed to get test data" txtE
                                                        runCheckResult answerTest psr txt

runCheckResult:: (Eq a, Show a) => (a -> Assertion) -> ResultParser a -> Text -> Assertion
runCheckResult answerTest = resultParserTest (checkParseResult answerTest)

checkParseResult :: (Eq a, Show a) => (a -> Assertion) -> Either (ParseError (Token Text) ErrorInfo) a -> Assertion
checkParseResult = either (\e -> assertFailure $ "Error in parser execution" <> show e)

resultParserTest :: (Eq a, Show a) => (Either (ParseError (Token Text) ErrorInfo) a -> Assertion) -> Parser a -> Text -> Assertion
resultParserTest test parser input =
                                     let
                                        parseResult = exeParser parser input
                                     in
                                        test parseResult

checkRight :: (Eq a, Show a) => Parser a -> Text -> Assertion
checkRight = resultParserTest (chk . isRight)
