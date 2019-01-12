-----------------------------------------------------------------------------
--
-- Module      :  MergeEngine
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
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeSynonymInstances #-}
--{-# LANGUAGE LambdaCase  #-}



module ReportGenerator where

import           Data.List           as List
import           Data.Maybe
import           Data.Monoid
import           Data.Text           as Text
import           Data.Time.Clock
import           Data.Time.LocalTime
import           Foundation.Extended (Truthy (..))
import           LamdahofUtils
import           ReportTemplates
import           ResultInterpreter
import           SubParsers
import           System.FilePath
import qualified Text.RE.Replace     as RegexReplace
import           Text.RE.TDFA.Text
import           Text.Read



parseStatsToReport :: FilePath -> IO (Either (FileParseError Char ErrorInfo) Text)
parseStatsToReport statsPath  = do
                                  resultText <- pathToIOEitherTxt statsPath
                                  time <- getZonedTime
                                  pure $ statsTextToReport statsPath time =<< resultText

statsTextToReport :: FilePath -> ZonedTime -> Text ->  Either (FileParseError Char ErrorInfo) Text
statsTextToReport statsPath zonedTime src =
                    let
                      maybeFull = (readMaybe (unpack src) :: Maybe FullInfo)
                    in
                      maybe (Left $ StatsReadFailure $ "Stats Read Failed " <> pack statsPath)
                            (Right . fullToReport statsPath zonedTime)
                            maybeFull

applyTotals :: Totals -> Text ->  Text
applyTotals tots template =
                    let
                      updateTotalWithNum tag n = replace tag  $ pack $ show n
                      updateTotal tag getter = updateTotalWithNum tag (getter tots)
                    in
                      updateTotal "#pendingCount" pendingTestCount
                        $ updateTotal "#skippedCount" skippedTestCount
                        $ updateTotal "#deferredCount" deferredTestCount
                        $ updateTotal "#failedCount" errorTestCount
                        $ updateTotalWithNum "#queriesCount" (possibleErrorTestCount tots + queryTestCount tots)
                        $ updateTotal "#passCount" passTestCount template

applyHeader :: FilePath -> Text ->  Text
applyHeader statsPath  = replace "#header" $ Text.replace "_" " " $ pack $ takeBaseName statsPath


applyFooter :: ZonedTime -> Text -> Text
applyFooter time = replace "#footer" (pack $ show time)

applyFailItems:: UTCTime -> Text -> Text
applyFailItems time = replace "#footer" (pack $ show time)

getStats :: LabledRawStat -> Maybe ExceptionList
getStats raw = case stats raw of
                        Stats e -> Just e
                        _       -> Nothing

combineExceptionLists :: ExceptionList -> ExceptionList -> ExceptionList
combineExceptionLists e0 e1 = ExceptionList {
    errors = errors e0 <> errors e1,
    possibleErrors = possibleErrors e0 <> possibleErrors e1,
    queries = queries e0 <> queries e1
}

baseJiraUrl :: Text
baseJiraUrl = "https://servicedesk.bobsPlace.com.au/browse/"

expandDefectIds:: Text -> Text
expandDefectIds src = RegexReplace.replaceAll ("${p}${s}<a href=\"" <> baseJiraUrl <> "${d}\">${d}</a>") $ src *=~ [re|${p}((!!|\?!|\?\?))${s}(( +))${d}([A-Za-z]{2,5}-[0-9]{1,6})|]

aggregateExceptions :: [ExceptionList] -> ExceptionList
aggregateExceptions = List.foldr combineExceptionLists emptyExceptionList

queriesAndPossibleErrors :: ExceptionList -> [[Text]]
queriesAndPossibleErrors el = possibleErrors el <> queries el

applyLineItems:: Text -> ExceptionList -> (ExceptionList -> [[Text]]) -> Text -> Text
applyLineItems itemsName el getter template  =
                    let -- !! Bug -- todo: escape this
                      makeListItem tx = "<li>" <> tx <> "</li>"
                      exList = (makeListItem . Text.unlines <$> getter el)
                    in
                      replace ("<!--#" <> itemsName <> "-->") (List.null exList ?
                                            "<div class=\"subtitle\">No " <> itemsName <> " in Test Run</div>" $
                                            "<ol id=\"failList\" class=\"recordsList\">" <> expandDefectIds (Text.unlines exList) <> "</ol>") template

-- ToDo: use a proper templating engine
fullToReport :: FilePath -> ZonedTime -> FullInfo -> Text
fullToReport statsPath zonedTime info = let
                      tots = totals info
                      allExceptions = aggregateExceptions $ catMaybes $ getStats <$> testRecs info
                     in
                      applyLineItems "Queries" allExceptions queriesAndPossibleErrors $
                      applyLineItems "Failures" allExceptions errors $
                      applyFooter zonedTime $
                      applyHeader statsPath $
                       applyTotals tots summaryTemplate
