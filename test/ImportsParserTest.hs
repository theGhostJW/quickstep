
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module ImportsParserTest where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.State
import           Data.Either
import           Data.List                 as List
import qualified Data.Map.Strict           as Map
import           Data.Monoid
import           Data.Set                  as Set
import           Data.Text                 as Text
import           Debug.Trace
import qualified Foundation.Extended       as F
import           GHC.Base                  (Alternative (..))
import           GHC.Unicode               hiding (Space)
import           ImportsParser             hiding (explicit)
import           LamdahofUtils             hiding (testParser)
import           LinkParser
import           LinkParserTypes
import           Numeric                   (readFloat, readHex, readSigned)
import           ParserTestHelpers
import           Paths_Parser
import           PlanExpander
import           SubParsers
import           System.Environment
import           Test.Tasty
import           Test.Tasty.HUnit.Extended
import           Test.Tasty.QuickCheck
import           Test.Tasty.TH
import           TestData
import           TestPlanParser
import           Text.Megaparsec
import           Text.Megaparsec.Error
import           Text.Megaparsec.Expr
import           Text.Megaparsec.Pos
import           Text.RawString.QQ

{-# ANN module ("HLint: ignore Use camelCase") #-}

testParser:: (Stream parseInput, ShowToken (Token parseInput), Eq parseResult, Show parseResult, ShowErrorComponent erroComp) =>
                Parsec erroComp parseInput parseResult ->  -- parser
                (parseResult -> Assertion) ->  -- test
                parseInput -> -- input
                Assertion
testParser parser assertion = testParserResult False id assertion parser

testImport = testParser parseImport


testImportError:: Parser Import ->
                                Text -> -- frag
                                Text -> -- input
                                Assertion
testImportError = testParserE True


emptyImports :: Set Text = Set.empty

isWildcard :: Import -> Bool
isWildcard imp = case imports imp of
                    Wildcard _ -> True
                    _          -> False

hidden :: Import -> Set Text
hidden imp = case imports imp of
                    Wildcard w -> w
                    _          -> emptyImports

explicit :: Import -> Set Text
explicit imp = case imports imp of
                    Wildcard actualHidden -> emptyImports
                    Explicit e            -> e

chkSet li =  chkEq (fromList li)

chkExplicitIs li =  chkSet li . explicit

chkHiden :: [Text] -> Import -> Assertion
chkHiden li =  chkSet li . ImportsParserTest.hidden

chkPath :: FilePath -> Import -> Assertion
chkPath path imp = chkEq path $ relativePath imp

unit_bad_import_not_lookup = testImportError parseImport ".lookup" "import * ..\\..\\shared.txt"

unit_simple_imports_wildcard = testImport (chkHiden [])
                                  "import * ..\\..\\shared.lookup\n  \t\rimport * ..\\..\\shared1.lookup\n\rimport * ..\\..\\shared1.lookup"

unit_wildcard_import_spaces_in_name = testImport (chkPath "Deduplication Functional.lookup") "import * Deduplication Functional.lookup"

unit_explicit_import_spaces_in_name = testImport (chkPath "DeduplicationExp Functional.lookup") "import (log_in book_up sign_out) DeduplicationExp Functional.lookup"

unit_simple_imports_blank_line_wildcard = testImport (chkHiden [])
                                  "import * ..\\..\\shared.lookup\nimport * ..\\..\\shared1.lookup\n\rimport * ..\\..\\shared1.lookup"

unit_simple_import_wildcard = testImport (chkHiden []) "import * ..\\..\\shared.lookup"

unit_simple_import_wildcard_with_hide = testImport(chkHiden ["log_in", "book_up", "sign_out"])
                                                      "import * ..\\..\\shared.lookup hide (log_in book_up sign_out)"

unit_simple_import_explicit = testImport (chkExplicitIs ["log_in", "book_up", "sign_out"])
                                                      "import (log_in book_up sign_out) ..\\..\\shared.lookup hide (log_in book_up sign_out)"

unit_simple_import_simple_bracket =
  let
     txt = "(log_in book_up sign_out)"
     check = chkEq (fromList ["log_in", "book_up", "sign_out"])
   in
     testParserResult False id check bracketVals txt

{- Integration Tests -}

unit_integration_valid_links = checkNoError parseTestFile [F.relfile|Valid Parent.plan|]

unit_integration_cyclic_links = checkIntegrationError "Cyclic dependencies are not allowed:" parseTestFile [F.relfile|Cyclic Parent.plan|]
                                *> checkIntegrationError "\\testData\\Cyclic Child.lookup" parseTestFile [F.relfile|Cyclic Parent.plan|]

unit_integration_shadow_links = checkIntegrationError "more than one import contains the lookup: hello_world" parseTestFile [F.relfile|Shadow Parent.plan|]
                                *> checkIntegrationError "Shadow Child.lookup and Shadow Child2.lookup" parseTestFile [F.relfile|Shadow Parent.plan|]

unit_integration_link_not_found = checkIntegrationError
                                      "\\testData\\Missing Child.lookup"
                                      parseTestFile [F.relfile|Missing Lookup.plan|]
                                  *> checkIntegrationError "Missing Lookup.plan" parseTestFile [F.relfile|Missing Lookup.plan|]


unit_integration_link_wrong_extension = checkIntegrationError "import extension must be .lookup" parseTestFile [F.relfile|Wrong Extension Lookup.plan|]
