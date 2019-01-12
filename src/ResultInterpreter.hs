{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ResultInterpreter where

import qualified Data.List           as L
import           Data.Monoid
import           Data.Text           as Txt hiding (find, foldl, foldr, init,
                                             last, null, scanr)
import           Foundation.Extended as F
import           LamdahofUtils
import           Prelude             (Read (..))
import qualified Prelude             as P
import           ResultParserShared

testStats :: RawTestResults -> FullInfo
testStats raw =
              let
               recs = transformStatsList raw
               tots = getTotals recs
              in
               FullInfo tots recs

getTotals :: LabledRawStats -> Totals
getTotals = foldr singleTestStats zeroTotals

incStat' :: Totals -> (Totals -> Integer) -> Integer
incStat' oldTotals getter = getter oldTotals  + 1

singleTestStats :: LabledRawStat -> Totals -> Totals
singleTestStats testInfo oldTotals =
                      let
                        tstStats = stats testInfo
                        incStat = incStat' oldTotals
                      in
                        case tstStats of
                          IsPending -> oldTotals { pendingTestCount = incStat pendingTestCount }
                          IsDeferred -> oldTotals { deferredTestCount = incStat deferredTestCount}
                          IsSkipped -> oldTotals { skippedTestCount = incStat skippedTestCount }
                          Stats elist -> updatePassFails oldTotals elist

updatePassFails :: Totals -> ExceptionList -> Totals
updatePassFails oldTotals excepts
                            | has errors = oldTotals { errorTestCount = incStat errorTestCount }
                            | has possibleErrors = oldTotals { possibleErrorTestCount = incStat possibleErrorTestCount }
                            | has queries = oldTotals { queryTestCount = incStat queryTestCount }
                            | otherwise = oldTotals { passTestCount = incStat passTestCount }
                          where
                            incStat = incStat' oldTotals
                            has :: (ExceptionList -> [[Text]]) -> Bool
                            has getter = not $ null (getter excepts)

zeroTotals :: Totals
zeroTotals = Totals {
   errorTestCount = 0,
   possibleErrorTestCount = 0,
   queryTestCount = 0,
   passTestCount = 0,
   pendingTestCount = 0,
   deferredTestCount = 0,
   skippedTestCount = 0
  }

transformStatsList :: RawTestResults -> LabledRawStats
transformStatsList raw = P.init $ L.scanr transformStats (LabledRawStat "Empty" IsPending) raw

emptyExceptionList :: ExceptionList
emptyExceptionList = ExceptionList {
                          errors = [],
                          possibleErrors = [],
                          queries = []
                        }

data FoldState = None | Errors | PossibleErrors | Queries
                    deriving (Eq, Ord, Show)

errorToken :: Text
errorToken = "!!"

queryToken :: Text
queryToken = "??"

possibleErrorToken :: Text
possibleErrorToken = "?!"

lineToFoldState :: Text -> FoldState
lineToFoldState txt
                    | textFound errorToken = Errors
                    | textFound possibleErrorToken = PossibleErrors
                    | textFound queryToken = Queries
                    | otherwise = None
                where
                    textFound = flip hasTextCaseSensitive txt


addLine :: ExceptionList -> FoldState -> Text -> ExceptionList
addLine excpt state txt
                      | state == Errors = excpt { errors = lineAppend errors }
                      | state == PossibleErrors = excpt { possibleErrors = lineAppend possibleErrors}
                      | state == Queries = excpt { queries = lineAppend queries}
                      | otherwise = excpt -- add nothing if we aren't in the right state
                   where
                      lineAppend :: (ExceptionList -> [[Text]]) -> [[Text]]
                      lineAppend getter  =
                                let
                                  list = getter excpt
                                in
                                  null list ? [[txt]] $ P.init list <> [P.last list <> [txt]] -- this could be slow revist with a view to cons and reverse

addItem :: ExceptionList -> FoldState -> Text -> ExceptionList
addItem excpt state txt
                      | state == Errors = excpt { errors = addTo errors}
                      | state == PossibleErrors = excpt { possibleErrors = addTo possibleErrors }
                      | state == Queries = excpt { queries = addTo queries }
                      | otherwise = excpt -- add nothing if we aren't in the right state
                  where
                      addTo :: (ExceptionList -> [[Text]]) -> [[Text]]
                      addTo getter = getter excpt <> [[txt]] -- this could be slow revist with a view to cons and reverse

updateException :: ExceptionList -> FoldState ->  FoldState -> Text -> ExceptionList
updateException excpt oldState newState txt =
                                let
                                  transformer = oldState == newState ? addLine $ addItem
                                in
                                  transformer excpt newState txt

updateExceptions :: (ExceptionList, FoldState) -> Text -> (ExceptionList, FoldState)
updateExceptions accum line =
                              let
                                excpt = fst accum
                                preFoldState = snd accum
                                newFoldState = lineToFoldState line
                              in
                                (updateException excpt preFoldState newFoldState line, newFoldState)

textToExceptionList :: [Text] -> ExceptionList
textToExceptionList lst = fst $ F.foldl' updateExceptions (emptyExceptionList, None) lst

fullStats :: [Text] -> TestStatsRaw
fullStats = Stats . textToExceptionList

hasText :: Text -> Text -> Bool
hasText needle hayStack = toLower needle `Txt.isInfixOf` toLower hayStack

hasTextCaseSensitive :: Text -> Text -> Bool
hasTextCaseSensitive needle hayStack = needle `Txt.isInfixOf` hayStack

bookmark :: Text
bookmark = "~~"

-- Have to scanRight
transformStats :: RawTestResult -> LabledRawStat -> LabledRawStat
transformStats raw prevLabledStat  =
          let
            testTitle = ResultParserShared.title raw
            testContent = content raw
            prevStat = stats prevLabledStat
          in
            LabledRawStat {
              ResultInterpreter.title = testTitle,
              stats = calcStats testContent prevStat
            }


calcStats :: Text -> TestStatsRaw -> TestStatsRaw
calcStats txt prevStats  =
          (prevStats == IsPending && not (hasTextCaseSensitive bookmark txt)) ?
            IsPending $
              let
                nlines = linesTrue txt
                idLine = find (Txt.isPrefixOf "id:") nlines
              in
                maybe
                   (fullStats nlines)
                   (\line -> hasText "defer" line ?
                                  IsDeferred $
                                    hasText "skip" line ?
                                      IsSkipped $
                                      fullStats nlines)
                   idLine


data ExceptionList = ExceptionList {
    errors         :: [[Text]],
    possibleErrors :: [[Text]],
    queries        :: [[Text]]
  }
  deriving (Eq, Ord, Show, Read)

data Totals = Totals {
   errorTestCount         :: Integer,
   possibleErrorTestCount :: Integer,
   queryTestCount         :: Integer,
   pendingTestCount       :: Integer,
   passTestCount          :: Integer,
   deferredTestCount      :: Integer,
   skippedTestCount       :: Integer
  }
   deriving (Eq, Ord, Show, Read)

data TestStatsRaw =
          IsDeferred |
          IsSkipped |
          IsPending |
          Stats ExceptionList
      deriving (Eq, Ord, Show, Read)

data FullInfo = FullInfo {
      totals   :: Totals,
      testRecs :: LabledRawStats
     }
     deriving (Eq, Ord, Show, Read)

data LabledRawStat = LabledRawStat {
        title :: Text,
        stats :: TestStatsRaw
      }
      deriving (Eq, Ord, Show, Read)

type LabledRawStats = [LabledRawStat]
