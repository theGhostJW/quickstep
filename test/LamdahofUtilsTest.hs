-----------------------------------------------------------------------------
--
-- Module      :  LamdahofUtilsTest
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
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module LamdahofUtilsTest where

import           Control.Monad.Identity
import           Control.Monad.Trans
import           Control.Monad.Trans.Maybe
import           Data.Either
import           Data.Maybe
import           Data.Monoid
import           Data.Text
import           Data.Text.Arbitrary
import           Foundation.Extended       (Truthy (..))
import qualified Foundation.Extended       as F
import           LamdahofUtils
import           Paths_Parser
import           Test.Tasty                ()
import           Test.Tasty.HUnit.Extended
import           Test.Tasty.QuickCheck
import           Test.Tasty.TH

{-
-- prop_length_append ::  [Int] -> [Int] -> Bool
-- prop_length_append as bs = length (as ++ bs) == length as + length bs
-- unit_check_length ::  Assertion
-- unit_check_length = 1 @=? length [()]
-}

{-# ANN module ("HLint: ignore Use camelCase":: String) #-}


unit_fullPath = chkEq "C:\\Automation\\QuickStep\\Parser\\Socket" =<< fullPath "C:\\Automation\\QuickStep\\Parser\\testData\\demo.plan" "../../Socket"
unit_fullPath_backed_out = chkEq "C:\\Socket" =<< fullPath "C:\\Automation\\QuickStep\\Parser\\testData\\demo.plan"  "../../../../../../../../../../../../../../../../../Socket"

_filePathsRecursive = do
                        paths <- findFilesMatchingRecursive "C:\\Automation\\QuickStep" "*.*"
                        print paths

_listFolders = do
                  paths <- listDirectories "C:\\Automation\\QuickStep"
                  print paths

prop_true_lines_unlines_inverse ::  Text -> Bool
prop_true_lines_unlines_inverse txt = txt == unlinesTrue (linesTrue txt)

-- note the extra line \n added
unit_unlines_sanity_check = chkEq "Hi there\nmy name is\nLuka\n" $ Data.Text.unlines (Data.Text.lines "Hi there\nmy name is\nLuka")
unit_unlines_sanity_check2 = "" @=? Data.Text.unlines (Data.Text.lines "")

unit_unlinesTrue = "Hi there\nmy name is\nLuka" ... unlinesTrue (linesTrue "Hi there\nmy name is\nLuka")
unit_unlinesTrue_trailing_newLine = "Hi there\nmy name is\nLuka\n" ... unlinesTrue (linesTrue "Hi there\nmy name is\nLuka\n")
unit_unlinesTrue2 = "" ... unlinesTrue (Data.Text.lines "")
unit_unlinesTrue3 = "\n" ... unlinesTrue (linesTrue "\n")

mystuff = unlinesTrue $ linesTrue "\n"

unit_safeFirst_empty = Nothing ... (safeFirst [] :: Maybe Int)
unit_safeFirst_just = Just 3 ... safeFirst [3, 4, 5]

unit_safeLast_empty = Nothing ...  (safeLast []  :: Maybe Int)
unit_safeLast_just = Just 9 ... safeLast [6, 7, 9]

dedupePlanPath = [F.relfile|Deduplication Functional.plan|]

unit_runTestDataFileTextIntegrationTest = runTestDataFileTextIntegrationTest dedupePlanPath hasLenMoreThanOne

hasLenMoreThanOne :: Text -> Assertion
hasLenMoreThanOne txt =  chk $ Data.Text.length txt > 1

unit_testDataText = do
                      testD <- testDataText dedupePlanPath
                      when (isLeft testD )
                        $  print $ def testD "Failed"
                      chk $ Data.Text.length (def testD "") > 1

unit_testDataFile = do
                      file <- testDataFile dedupePlanPath
                      chk $ isRight file

unit_maybe_def_with_nothing = def Nothing 3 ... 3

unit_maybe_def_with_something = def (Just 5) 7 ... 5

unit_either_def_with_error = def (Left "Bad")  3 ... 3

unit_either_def_with_success = def (Right 5) 7 ... 5

unit_hasA_true = chk $ hasA "apple" 'a'

unit_hasA_false = chkFalse $ hasA "apple" 'b'

unit_ternary_true = chk (True ? True $ False)

unit_ternary_false = chk (False ? False $ True)
