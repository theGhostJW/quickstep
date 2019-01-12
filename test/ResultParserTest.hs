{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module ResultParserTest where

import           Control.Monad
import           Control.Monad.State
import           Data.Either
import           Data.Maybe
import           Data.Monoid
import           Data.Text
import qualified Foundation.Extended       as F
import           LamdahofUtils
import           ParserTestHelpers
import           ResultParser
import           ResultParserShared
import           ResultParserTestHelpers
import           SubParsers
import           Test.Tasty
import           Test.Tasty.HUnit.Extended
import           Test.Tasty.QuickCheck
import           Test.Tasty.TH
import           TestData
import           Text.Megaparsec
import           Text.Megaparsec.Error

{-# ANN module "HLint: ignore Use camelCase" #-}

unit_test_edge4 =
  let
    parser = titleEdge
    target = "---- "
    ck = chkEq "----"
  in
    runCheckResult ck parser target

unit_test_title =
  let
    parser = testTitle
    target = "---- Hello ----"
    ck = chkEq "Hello"
  in
    runCheckResult ck parser target

unit_test_title_with_dash =
  let
    parser = testTitle
    target = "---- Hello - There ----"
    ck = chkEq "Hello - There"
  in
    runCheckResult ck parser target

unit_single_testcase =
  let
    parser = rawTestCase
    target = testCaseTxt
    ck = chkContains "---" . content
  in
    runCheckResult ck parser target

unit_single_testunit_from_multi =
  let
    parser = rawTestCase
    target = multiCaseTxt
    ck r = chkFalse ("----" `Data.Text.isInfixOf` content r)
  in
    runCheckResult ck parser target

unit_raw_test_cases =
  let
    parser = rawTestCases
    target = multiCaseTxt
    ck r = chkEq 2 $ Prelude.length r
  in
    runCheckResult ck parser target

unit_raw_test_cases_integration  =
  let
    parser = rawTestCases
    ck r = chkEq 4 $ Prelude.length r
  in
    runCheckResultOnTestDataFile ck parser [F.relfile|Another_Dummy_Test_Run.rslt|]

firstRecContent :: RawTestResults -> Text
firstRecContent rs = content $ Prelude.head rs

unit_raw_test_cases_with_preamble =
  let
    parser = rawTestCases
    target = multiCaseTxtWithPreamble
    ck r = chk ((8 == Prelude.length r) && not ("preamble" `Data.Text.isInfixOf` firstRecContent r))
  in
    runCheckResult ck parser target
