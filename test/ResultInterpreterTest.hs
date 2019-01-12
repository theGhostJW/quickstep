{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ResultInterpreterTest where

import           Control.Monad
import           Data.Either
import           Data.List                 as List
import           Data.Maybe
import           Data.Monoid
import           Data.Text
import           Foundation.Extended       as F
import           LamdahofUtils
import           ParserTestHelpers
import           Paths_Parser
import qualified Prelude                   as P
import           ResultInterpreter
import           ResultParser
import           ResultParserShared
import           ResultParserTestHelpers
import           Test.Tasty
import           Test.Tasty.HUnit.Extended
import           Test.Tasty.QuickCheck
import           Test.Tasty.TH
import           TestData
import           Text.Megaparsec
import           Text.Megaparsec.Error

{-# ANN module ("HLint: ignore Use camelCase" :: P.String) #-}

hasTestCountOf ::  Int -> FullInfo -> Assertion
hasTestCountOf n info = chkEq n $ List.length $ testRecs info

hasTotalsOf ::  Totals -> FullInfo -> Assertion
hasTotalsOf tot info = chkEq tot $ totals info

canSerialize :: FullInfo -> Assertion
canSerialize info = let
                      newStr = P.show info
                      newVal = P.read newStr
                    in
                      info ... newVal

unit_test_count = runCheckInterpreter (hasTestCountOf 2) multiCaseTxt

unit_test_totals =
    let
      totals = zeroTotals {
                            pendingTestCount = 1,
                            queryTestCount = 1
                          }
    in
      runCheckInterpreter (hasTotalsOf totals) multiCaseTxt

unit_test_totals_big =
    let
      totals = zeroTotals {
                           errorTestCount = 1,
                           possibleErrorTestCount = 1,
                           queryTestCount = 1,
                           passTestCount = 1,
                           pendingTestCount = 2,
                           deferredTestCount = 1,
                           skippedTestCount = 1
                  }
                  in
                    runCheckInterpreter (hasTotalsOf totals) multiCaseTxtWithPreamble

integrationTest :: Text -> Assertion
integrationTest txt =   let
                          totals = zeroTotals {
                                                         errorTestCount = 2,
                                                         possibleErrorTestCount = 1,
                                                         queryTestCount = 1,
                                                         passTestCount = 8,
                                                         pendingTestCount = 0,
                                                         deferredTestCount = 1,
                                                         skippedTestCount = 1
                                              }
                                               in
                                                runCheckInterpreter (hasTotalsOf totals) txt

unit_test_queried_list_integration_bug = runTestDataFileTextIntegrationTest [F.relfile|Demo_Test_Run.rslt|] integrationTest

unit_not_handling_mutli_hypens_out_of_title_bug =
  let
    totals = zeroTotals {
                         errorTestCount = 0,
                         possibleErrorTestCount = 1,
                         queryTestCount = 0,
                         passTestCount = 0,
                         pendingTestCount = 0,
                         deferredTestCount = 0,
                         skippedTestCount = 0
                }
                in
                  runCheckInterpreter (hasTotalsOf totals) testCaseWithHyphens


unit_test_totals_serialization = runCheckInterpreter canSerialize multiCaseTxtWithPreamble
