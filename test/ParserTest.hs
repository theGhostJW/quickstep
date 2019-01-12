-----------------------------------------------------------------------------
--
-- Module      :  MainTest
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

{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module ParserTest where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.State
import           Data.Either
import           Data.List
import qualified Data.Map.Strict           as Map
import           Data.Monoid
import           Data.Text
import           Debug.Trace
import qualified Foundation.Extended       as F
import           GHC.Base                  (Alternative (..))
import           GHC.Unicode               hiding (Space)
import           LamdahofUtils
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

planMissingArgInParam = [r|

_ftp_input _baseFile _dateFormat
  # create and upload the ftp input file:
    # copy and rename to current as per the current date (_dateFormat) `aux_file fileName: _baseFile
    # note the name: ??
    # copy to amex
    # check that there is the only *.mon file in the FTP directory if there is an old file there delete it

_stuff_in_stuff
  stuff blahh blahh

---- Test Name ----

  ftp_input crap: stuff
            crapa: <MMYY>

  end
|]


unit_missing_partial_link_arg =
  let
    plan = planMissingArgInParam
    checkResult = chkContains "undeclared param name: crap"
  in
    parseExpandPlanFailureTest checkResult plan

{- Double Escaped Link -}

-- for some reason quas quotes not working
escapedLinkRepeated = [r|
  Hello World
  `tst_server

  tst_server
blahh
|]

unit_double_escaped =
 let
    linkTxt = ""
    plan = escapedLinkRepeated
    checkResult = chkContains "tst_server"
  in
    parseExpandFileContentTest checkResult plan linkTxt

{- Embeded test links -}

verySimplePlanWithInternalLink = toRealTabSpace [r|_link_one
<Tab>Hello World

blahh
|]


unit_very_simple_plan_embedded_link_definition =
 let
    linkTxt = ""
    plan = verySimplePlanWithInternalLink
    checkResult = chkEq "blahh"
  in
    parseExpandFileContentTest checkResult plan linkTxt

simplePlanWithInternalLink = toRealTabSpace [r|_link_one
<Tab>Hello World

link_one
|]


unit_simple_plan_embedded_link_definition =
 let
    linkTxt = ""
    plan = simplePlanWithInternalLink
    checkResult = chkEq "Hello World"
  in
    parseExpandFileContentTest checkResult plan linkTxt


consecutiveLinkPlan = toRealTabSpace [r|_link_one
<Tab>Hello

_link_two
<Tab>World

---- Full Plan ----
link_one link_two
|]

unit_consecutive_links_plan_embedded_link_definition =
 let
    linkTxt = ""
    plan = consecutiveLinkPlan
    checkResult = chkEq "---- Full Plan ----\nHello World"
  in
    parseExpandFileContentTest checkResult plan linkTxt

nonConsecutiveLinkPlan = toRealTabSpace [r|_link_one
<Tab>Hello

---- Full Plan ----
_link_two
<Tab>World

link_one link_two
|]

unit_non_consecutive_links_plan_embedded_link_definition =
 let
    linkTxt = ""
    plan = nonConsecutiveLinkPlan
    checkResult = chkEq "---- Full Plan ----\nHello World"
  in
    parseExpandFileContentTest checkResult plan linkTxt

nonConsecutiveLinkWithParamsPlan = toRealTabSpace [r|_link_one _w1 _w2
<Tab>_w1 _w2

---- Full Plan ----
_link_two _sfx
<Tab>World _sfx

link_one w1: Hello
         w2: Cool
link_two sfx: s
|]

unit_non_consecutive_links_with_params_plan_embedded_link_definition =
 let
    linkTxt = ""
    plan = nonConsecutiveLinkWithParamsPlan
    checkResult = chkEq "---- Full Plan ----\nHello Cool\nWorld s"
  in
    parseExpandFileContentTest checkResult plan linkTxt

nonConsecutiveLinkWithParamsAndLookupPlan = nonConsecutiveLinkWithParamsPlan <> pack "\tlogin_"

unit_non_consecutive_links_with_params_plan_embedded_link_and_lookup_definition =
 let
    linkTxt = "_login_\n\t# log into CYC using login details"
    plan = nonConsecutiveLinkWithParamsAndLookupPlan
    checkResult = chkEq "---- Full Plan ----\nHello Cool\nWorld s\n\t# Log into CYC using login details"
  in
    parseExpandFileContentTest checkResult plan linkTxt


nesteLinksPlan = toRealTabSpace [r|_link_one _w1 _w2
<Tab>_w1 _w2

---- Full Plan ----
_link_two _sfx
<Tab>World _sfx

_link_three _s _w1 _w2
<Tab>link_one w1: _w1
              w2: _w2
<Tab>link_two sfx: _s
<Tab>login_

link_three s: Rocks
           w1: Hello
           w2: Cool
|]

unit_nested_links_plan_embedded_link_definition =
 let
    linkTxt = "_login_\n\t# log into CYC using login details"
    plan = nesteLinksPlan
    checkResult = chkEq "---- Full Plan ----\nHello Cool\nWorld Rocks\n# Log into CYC using login details"
  in
    parseExpandFileContentTest checkResult plan linkTxt

unit_embedded_duplicate_link_failure_definition =
 let
    linkTxt = "_link_two\n\t# this is a duplicate"
    plan = nonConsecutiveLinkWithParamsPlan
    checkResult = chkContains "link_two" >> chkContains "re-declared"
  in
    parseExpandFailureTest checkResult plan linkTxt

unit_read_link_name_declaration = simpleParseTest readLinkNameInDeclaration (chkEq "login_")  "_login_"
unit_read_link_name_declaration2 = simpleParseTest readLinkNameInDeclaration (chkEq "login_to_site")  "_login_to_site"

checkName (l :: LinkDefinition) = chkEq "login_" $ linkName $ declaration l

unit_simple_link_def = simpleParseTest linkDefinition checkName "_login_\n\t# log into CYC using login details"

textToLinkDefsSingular :: Text -> LinkDefinitions
textToLinkDefsSingular linkTxt = let lDef = elementFromTextNoLinks linkDefinition linkTxt
                                     lName = linkName $ declaration lDef
                                  in
                                      Map.singleton lName lDef

logInLink :: LinkDefinitions
logInLink = textToLinkDefsSingular "_login_\n\t# log into CYC using login details"

noDoubleSpace :: Text -> Assertion
noDoubleSpace txt = chkFalse $ Data.Text.isInfixOf "  " txt

unit_nested_links_eats_space = parseExpandWithLinksTest linkedTextParser logInLink noDoubleSpace "# login_"

unit_link_should_fail =
  let
     linkTxt = ""
     plan = "\nindividual_enquiry_matched"
     checkResult = chkContains "undeclared"
   in
     parseExpandFailureTest checkResult plan linkTxt

unit_spaces_not_tab_in_lookup_definition =
 let
    linkTxt = "_link_one\n  link def with no tab"
    plan = "link_one"
    checkResult = chkContains "link def"
  in
    parseExpandFileContentTest checkResult plan linkTxt

unit_no_tab_on_lookup_def =
  let
     linkTxt = "_link_one\n link def with no tab"
     plan = "link_one"
     checkResult = chkContains "<Tab>"
   in
     parseExpandFailureTest checkResult plan linkTxt


tabbyParams = toRealTabSpace [r|
_link_one<Tab>_param<Space>
<Tab>Hi

_link_two<Tab>_param<Tab>_param1<Space>_paramOther
<Tab>There
|]

unit_lookup_params_with_tab_spaced =
  let
     linkTxt = tabbyParams
     plan = "link_one param: 1\nlink_two param: p\nparam1: 1\nparamOther: 2"
     checkResult = chkEq "Hi\nThere"
   in
     parseExpandFileContentTest checkResult plan linkTxt

multiParamsLookUp = toRealTabSpace [r|
_link_one _param _param2
<Tab>Hi
|]

multiParamsPlan = toRealTabSpace [r|
<Tab>link_one  param: hello

    blahh
|]

unit_missing_link_arg_error =
  let
    linkTxt = multiParamsLookUp
    plan = multiParamsPlan
    checkResult = chkContains "param2"
  in
    parseExpandFailureTest checkResult plan linkTxt

spacyParams = toRealTabSpace [r|
_link_one _param<Space>
<Tab>Hi

_link_two _param<Tab>
<Tab>There
|]

unit_lookup_params_with_trailing_whitespace =
  let
     linkTxt = spacyParams
     plan = "link_one param: 1\nlink_two param: 2"
     checkResult = chkEq "Hi\nThere"
   in
     parseExpandFileContentTest checkResult plan linkTxt


unit_linking_preserves_tabs =
   let
      linkTxt = "_the_steps\n\tline 1\n\tline 2"
      plan = "\tthe_steps"
      checkResult = chkEq "\tline 1\n\tline 2"
    in
      parseExpandFileContentTest checkResult plan linkTxt

spacyLookups = toRealTabSpace [r|
_link_one
<Tab>Hi
<Space>

_link_two
<Tab>There
|]

unit_spacy_lookup_delimiters =
   let
      linkTxt = spacyLookups
      plan = "link_two"
      checkResult = chkEq "There"
    in
      parseExpandFileContentTest checkResult plan linkTxt

multiTabs = toRealTabSpace [r|
_link_one
<Tab># link one

_link_two
<Tab><Tab>link two - child tabbed in 2

_link_three
<Tab><Tab># link three
<Tab>link_two
|]

multiTabsCall = toRealTabSpace [r|
<Tab>link_one
<Tab><Tab>link_two
<Tab><Tab><Tab>link_three
|]

multiTabsExpected = strip $ toRealTabSpace [r|
<Tab># Link one
<Tab><Tab><Tab>link two - child tabbed in 2
<Tab><Tab><Tab><Tab># Link three
<Tab><Tab><Tab><Tab>link two - child tabbed in 2
|]

unit_mult_link_tabs =
   let
      linkTxt = multiTabs
      plan = multiTabsCall
      checkResult = chkContains multiTabsExpected
    in
      parseExpandFileContentTest checkResult plan linkTxt

defectLinkDefinitionSimple = doubleSpaceToTab [r|copy_file _fileName _server
    # copy fileName from ..\supporting files\fileName to _server\dropFolder
    => the file should dissapear within around 30 seconds as it is picked up by the service
|]

unit_check_link_file_single = checkNoParserError linkFileContent defectLinkDefinitionSimple

unit_single_unnested_link =
   let
      linkTxt = "_hello_there _name\n\thello _name"
      plan = "hello_there name: world"
      checkResult = chkEq "hello world"
   in
      parseExpandFileContentTest checkResult plan linkTxt

nestedLinksSimple = doubleSpaceToTab [r|_hello_there _name
    hello _name

_hello_call _name
    hello_there name: _name
|]

unit_nested_link_params_pass_through_simple =
   let
      linkTxt = nestedLinksSimple
      plan = "hello_call name: Dolly"
      checkResult = chkContains "hello Dolly"
   in
      parseExpandFileContentTest checkResult plan linkTxt

nestedLinks = doubleSpaceToTab [r|_copy_file _fileName _server _toFileName
    # copy _fileName from ..\supporting files\_fileName to _server\dropFolder\_toFileName
    => the file should dissapear within around 30 seconds as it is picked up by the service

_copy_file_same_name _fileName _server
   copy_file fileName: _fileName
             server: _server
             toFileName: _fileName

_copy_any_file _server
   copy_file_same_name fileName: toAndFrom
                        server: _server
|]

unit_nested_link_params_pass_through =
   let
      linkTxt = nestedLinks
      plan = "copy_any_file server: testServer"
      checkResult = chkContains "# Copy toAndFrom from ..\\supporting files\\toAndFrom to testServer\\dropFolder\\toAndFrom"
   in
      parseExpandFileContentTest checkResult plan linkTxt

nestedScrambleParamsSimple = doubleSpaceToTab [r|_copy_file _server _file
    toFile _file server _server

_copy_file_switch_params _file _server
   copy_file server: _file
             file: _server
|]

unit_nested_link_params_pass_through_scrambled_simple =
   let
      linkTxt = nestedScrambleParamsSimple
      -- the lookups in this case unscramble the incorrectly entered params
      plan = "copy_file_switch_params server: toFileVal\n\t\
                                     \file: serverVal\n\t"
      checkResult = chkContains "toFile toFileVal server serverVal"
   in
      parseExpandFileContentTest checkResult plan linkTxt

nestedScrambleParams = doubleSpaceToTab [r|_copy_file _fileName _toFile _server
    fileName _fileName toFile _toFile server _server

_copy_file_same_name _toFile _server _fileName
   copy_file fileName: _server
             server: _toFile
             toFile: _fileName

_copy_any_file_scramble _server _fileName _toFile
   copy_file_same_name fileName: _fileName
                        server: _server
                        toFile: _toFile
|]

unit_nested_link_params_pass_through_scrambled =
   let
      linkTxt = nestedScrambleParams
      -- the lookups in this case unscramble the incorrectly entered params
      plan = "copy_any_file_scramble server: fileSentAsServer\n\t\
                                  \fileName: toFileSentAsFileName\n\t\
                                  \toFile: serverSentAsToFile\n\t"
      checkResult = chkContains "fileName fileSentAsServer toFile toFileSentAsFileName server serverSentAsToFile"
   in
      parseExpandFileContentTest checkResult plan linkTxt

unit_undefined_link_question_mark =
   let
      linkTxt = "_log_in\n\t??\r\n\r\n"
      plan = "\n# log_in\r\n\r\n"
      checkResult = chkContains "??log_in??"
   in
      parseExpandFileContentTest checkResult plan linkTxt


unit_link_params_with_punctuation =
   let
      linkTxt = "_log_in\n\tlog in"
      plan = "log_in' to server"
      checkResult = chkEq "log in' to server"
   in
      parseExpandFileContentTest checkResult plan linkTxt

unit_same_line_block_quotes_link =
   let
      linkTxt = ""
      plan = "this is a blocked text plan \"\"\"log_in\"\"\" more"
      checkResult = chkEq "this is a blocked text plan log_in more"
   in
      parseExpandFileContentTest checkResult plan linkTxt

unit_weird_quote_in_text =
   let
      linkTxt = ""
      plan = "which will alter the way the Add’ action type"
      checkResult = ("which will alter the way the Add’ action type" ...)
   in
      parseExpandFileContentTest checkResult plan linkTxt


simpleLinkDefText = doubleSpaceToTab [r|_link_one _param1
    this is p1 _param1 yea!
|]

simpleLink :: LinkState
simpleLink = linkedLinkState $ textToLinkDefsSingular simpleLinkDefText

unit_check_link_reference = checkNoParserErrorWithLinks (linkRef []) simpleLink "link_one param1: blah2"


nestedLinkText = addTabs $ pack [r|link_one _param1
    this is p1 _param1 yea!

link_two _param2
  link_one param1: _param2
  more stuff line 2
  more stuff line 3

link_three
  # link_two param2: feed through param
  => verify it
|]

unit_check_link_file = checkNoParserError linkFileContent nestedLinkText

dedupePlanPath = [F.relfile|Deduplication Functional.plan|]

unit_integration_parseQuickStepFile = checkNoError parseTestFile dedupePlanPath

unit_integration_full_pass = readParseExpandSave dedupePlanPath

noLinkEqCase plan expectedText = parseExpandFileContentTest (chkEq expectedText) plan ""

{- does not work invalid byte sequence - some chars just cant be used
probably related to encoding
-- case long hyphen = do
                      testD <- testDataText getLibDir "badSymbol.txt"
                      when (isLeft testD )
                        $  print $ def testD "Failed"
                      let chr = Data.Text.head $ def testD "!!"
                          messsage = "the char read was: " ++ [chr]
                      print messsage
                      chk $ isPunctuationChrExcludingUnderscore chr
-}



unit_simple_long_hyphen = noLinkEqCase "* Active Trade Participants – Year To Year " "* Active Trade Participants – Year To Year "
unit_simple_single_word = noLinkEqCase "'Hi" "'Hi"
unit_simple_single_word_escaped = noLinkEqCase "`Hi" "Hi"
unit_simple_single_escape = noLinkEqCase "`log_in" "log_in"
unit_simple_single_escape1 = noLinkEqCase "'`NZ_Sample_Patient_*.json" "'NZ_Sample_Patient_*.json"
unit_simple_single_escape2 = noLinkEqCase "`'NZ_Sample_Patient_*.json" "'NZ_Sample_Patient_*.json"

unit_very_simple_block_escape = checkNoParserError consumeToggleBlockEsc "\"\"\""
unit_very_simple_block_escape_2 = checkNoParserError consumeToggleBlockEsc "\"\"\"\blahh"
unit_very_simple_block_escape_no_change = checkNoParserError consumeToggleBlockEsc "\"\""


unit_simple_block_escape =
   let
      linkTxt = ""
      plan = "this is \"\"\"esc_aped Text yea"
      checkResult = chkEq "this is esc_aped Text yea"
   in
      parseExpandFileContentTest checkResult plan linkTxt
