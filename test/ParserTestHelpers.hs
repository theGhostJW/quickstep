-----------------------------------------------------------------------------
--
-- Copyright   :
-- License     :  AllRightsReserved
-- Module      :  ParserTestItems
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
--
--
-----------------------------------------------------------------------------
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module ParserTestHelpers  where

import           Control.Monad
import           Control.Monad.State
import           Data.Either.Combinators
import qualified Data.Map.Strict           as Map
import           Data.Monoid
import           Data.Text
import           Debug.Trace
import qualified Foundation.Extended       as F
import           LamdahofUtils
import           LinkParser
import           LinkParserTypes
import           Paths_Parser
import           PlanExpander
import           SubParsers
import           Test.Tasty.HUnit.Extended
import           TestPlanParser
import           Text.Megaparsec
import           Text.Megaparsec.Char


debugParser :: Parser a -> Parser a
debugParser p = do
                  remaining <- lookAhead $ many anyChar
                  traceM ("consumed: " ++ show remaining)
                  p

checkNoParserErrorWithLinks :: (Eq r, Show r) => LinksParser r -> LinkState -> Text -> Assertion
checkNoParserErrorWithLinks parser ps input = do
                                  let parseResult = exeParser (runStateT parser ps) input
                                  chkRight NoPrint parseResult

checkNoParserError :: (Eq r, Show r) => LinksParser r -> Text -> Assertion
checkNoParserError parser = checkNoParserErrorWithLinks parser initialLinkState

data Print = Print | NoPrint deriving Eq

chkRight :: (Show l, Show r) => Print -> Either l r -> Assertion
chkRight wantPrint eith = do
                        when (isLeft eith)
                           $ print eith

                        when (wantPrint == Print && isRight eith)
                          $ print ("ALL GOOD!!! \n" <> show eith)

                        chk $ isRight eith

elementFromTextNoLinks :: (Eq a, Show a) =>  LinksParser a -> Text -> a
elementFromTextNoLinks psr txt = fst $ parserResultUnsafe (runStateT psr initialLinkState) txt

parseTestFile :: F.RelFile -> IO (Either (FileParseError Char ErrorInfo) ElementBlock)
parseTestFile rf = do
                    ethTF <- testDataFile rf
                    F.eitherf ethTF
                      (pure . Left . ReadError)
                      (parseQuickStepFile . F.toFilePath)

checkNoError :: (Show l, Show r) => (a -> IO (Either l r)) -> a -> Assertion
checkNoError runner inputTxt = do
                                  rslt <- runner inputTxt
                                  chkRight NoPrint rslt

checkIntegrationError ::  (Show l, Show r) => Text -> (a -> IO (Either l r)) -> a -> Assertion
checkIntegrationError fragment runner inputTxt =
                              chkLeftContains fragment =<< runner inputTxt

testFullParse :: (FilePath, FilePath) -> Assertion
testFullParse (srcPath, destDir) = do
                                     resultPathEith <- parseCopyTestPlan srcPath destDir
                                     chk $ isRight resultPathEith

readParseExpandSave :: F.RelFile -> Assertion
readParseExpandSave file = do
                              pathEith <- testDataFile file
                              tempFldr <- tempDir
                              let parseArgs  = do
                                                  planPath <- pathEith
                                                  destDir <- tempFldr
                                                  pure (F.toFilePath planPath, F.toFilePath destDir)

                              either (fail . show) testFullParse parseArgs

parseExpandFileContentTest :: (Text -> Assertion) -> Text -> Text -> Assertion
parseExpandFileContentTest = parseExpandTest linkedTextParser


linkPsr :: Text -> Either (ParseError Char ErrorInfo) LinkDefinitions
linkPsr = parseLookUp "LinkTest" Map.empty

parseExpandTest :: LinksParser ElementBlock -> (Text -> Assertion) -> Text -> Text -> Assertion
parseExpandTest linksParser textAssert planText linkText =
         either (assertFailure . show)
                (\links -> parseExpandWithLinksTest linksParser links textAssert planText)
                (linkPsr linkText)

parseExpandPlanFailureTest :: (Text -> Assertion) -> Text ->  Assertion
parseExpandPlanFailureTest funcTest plan  = parseExpandFailureTest funcTest plan ""

parseExpandFailureTest :: (Text -> Assertion) -> Text -> Text -> Assertion
parseExpandFailureTest = parseFailureTest linkedTextParser

parseFailureTest :: LinksParser ElementBlock -> (Text -> Assertion) -> Text -> Text -> Assertion
parseFailureTest linksParser failTextAssert planText linkText =
  let
     applyAssertion :: ParseError Char ErrorInfo -> Assertion
     applyAssertion = failTextAssert . pack . parseErrorPretty
  in
     case linkPsr linkText of
        Left err -> applyAssertion err
        Right links ->
                      let
                        linkEsc = linkedLinkState links
                        parser = runStateT linksParser linkEsc
                        prsResult = exeParser parser planText
                      in
                        case prsResult of
                          Left err -> applyAssertion err
                          Right _ -> assertFailure "Expected parser failure but parser succeeded"

parseExpandWithLinksTest :: LinksParser ElementBlock -> LinkDefinitions -> (Text -> Assertion) -> Text -> Assertion
parseExpandWithLinksTest linksParser lnkDefs textAssert input =
      let
         linkEsc = linkedLinkState lnkDefs
         parser = runStateT linksParser linkEsc
         psrResultToText psrResult = expandBody $ fst psrResult
      in
         testParserResult False psrResultToText textAssert parser input


simpleParseTest :: (Eq a, Show a) => LinksParser a -> (a -> Assertion) -> Text -> Assertion
simpleParseTest linksParser textAssert input =
     let
        parser = runStateT linksParser initialLinkState
     in
        testParserResult False fst textAssert parser input
