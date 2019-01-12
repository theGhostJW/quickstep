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

module ReportGeneratorTest  where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.State
import           Data.Either.Combinators
import           Data.List
import qualified Data.Map.Strict           as Map
import           Data.Monoid
import           Data.Text
import           Data.Tuple
import           Debug.Trace
import           GHC.Base                  (Alternative (..))
import           GHC.Unicode
import           LamdahofUtils
import           LinkParser
import           LinkParserTypes
import           Numeric                   (readFloat, readHex, readSigned)
import           Paths_Parser
import           PlanExpander
import           ReportGenerator
import           SubParsers
import           System.Environment
import           Test.Tasty
import           Test.Tasty.HUnit.Extended
import           Test.Tasty.QuickCheck
import           TestData

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}

baseToRef :: Text -> Text -> Text
baseToRef  defectId src = "<a href=\"" <> baseJiraUrl <> defectId <> "\">" <> defectId <> "</a>"

-- only works for simple cases
hrefToExpected :: Text -> Text -> Text
hrefToExpected defectId src =
                  let
                    expected = baseToRef defectId src
                  in
                    replaceOne defectId expected src

unit_href_test_test_func_positive_oy = chkEq "!! <a href=\"https://servicedesk.bobsPlace.com.au/browse/EN-234\">EN-234</a>" (hrefToExpected "EN-234" "!! EN-234")
unit_href_test_test_func_positive_repeat_oy = chkEq "!! <a href=\"https://servicedesk.bobsPlace.com.au/browse/EN-234\">EN-234</a> EN-234" (hrefToExpected "EN-234" "!! EN-234 EN-234")
unit_href_test_test_func_negative_oy = chkEq "!! EN-234" (hrefToExpected "EN-233" "!! EN-234")

linkCheck :: Text -> Text -> Assertion
linkCheck defect line =
                  let
                    actual = expandDefectIds line
                    expected = hrefToExpected defect line
                  in
                    chkEq expected actual


unit_href_matches_tests_simple_oy = linkCheck "EN-234" "!! EN-234"
unit_href_matches_tests_repeat_oy = linkCheck "EN-234" "!! EN-234  EN-234"
unit_href_matches_tests_trailing_oy = linkCheck "EN-234" "!! EN-234  some more text blahh"
unit_href_matches_tests_trailing_possible_oy = linkCheck "EN-234" "?! EN-234 some more text blahh"
unit_href_matches_tests_trailing_quert_oy = linkCheck "EN-234" "?? EN-234 some more text blahh"


unit_href_matches_wrong_pattern_oy =  chkEq "?? EN-a34 some more text blahh" $ expandDefectIds "?? EN-a34 some more text blahh"
unit_href_matchesmulti_oy =  chkEq "!! <a href=\"https://servicedesk.bobsPlace.com.au/browse/EN-234\">EN-234</a> some more text ?! <a href=\"https://servicedesk.bobsPlace.com.au/browse/EANT-78965\">EANT-78965</a> blahh" $ expandDefectIds "!! EN-234 some more text ?! EANT-78965 blahh"
