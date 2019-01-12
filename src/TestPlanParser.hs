{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}


module TestPlanParser where

import           Control.Monad.State
import           Data.Either.Combinators
import           Data.List               as List
import           Data.Map.Strict
import qualified Data.Map.Strict         as Map
import           Data.Monoid
import           Data.Set                as Set
import           Data.Text               as Text
import           Foundation.Extended
import           ImportsParser
import           LamdahofUtils           as Utils
import           LinkParser
import           LinkParserTypes
import           ParserRunner
import           PlanExpander
import           Prelude                 (IOError)
import           SubParsers
import           System.Directory        as D
import           System.FilePath         as P
import           Text.Megaparsec

-- resolveImports :: Parser [Import] -> IO (Either (FileParseError Char ErrorInfo) LinkDefinitions)
-- resolveImports = undefined
--
-- impParser :: Parser  -> IO (Either (FileParseError Char ErrorInfo) [Import])



allImportLinks :: FilePath -> Either (FileParseError Char ErrorInfo) LinkDefinitions -> Text -> Either (FileParseError Char ErrorInfo) LinkDefinitions
allImportLinks path lnkDefs txt = do
                                    links <- lnkDefs
                                    case parseLookUp path links txt of
                                      Left e  -> Left $ ParserError e
                                      Right r -> pure r

refineLinkDefs :: FilePath -> Import -> LinkDefinitions -> Either (FileParseError Char ErrorInfo) LinkDefinitions
refineLinkDefs path imprt defs = let
                                  (_, activeSet)       =  case imports imprt of
                                                                Wildcard hideSet -> (True, hideSet)
                                                                Explicit include -> (False, include)
                                  invalid = Set.toList $ Set.filter (`Map.notMember` defs) activeSet
                                 in
                                  List.length invalid /= 0 ?
                                    (
                                      Left $ IllegalImport $ pack path <>
                                          ": listed import or hidden items for " <> pack (relativePath imprt) <>
                                          " are not present in import file: " <>
                                          Text.intercalate ", "  invalid
                                    )  $
                                    Right $
                                       case imports imprt of
                                         Wildcard hideSet -> Set.foldl' (flip Map.delete) defs hideSet
                                         Explicit include -> Set.foldl' (\m k -> Map.insert k (defs ! k) m) Map.empty include

resolveImport :: FilePath -> [FilePath] -> Import -> IO (Either (FileParseError Char ErrorInfo) LinkDefinitions)
resolveImport callingFile visited imp = do
                                          let
                                            badImportErr :: Text -> IO (Either (FileParseError Char ErrorInfo) LinkDefinitions)
                                            badImportErr t = pure (Left . IllegalImport $ t)

                                          fullImportPath <- Utils.fullPath callingFile $ relativePath imp
                                          exists <- D.doesFileExist fullImportPath
                                          not exists ? (
                                            let
                                              msg =  pack $ "Referenced lookup file does not exists or the format is invalid. Referencing Path: " <> fullImportPath <> " <FROM> " <> callingFile
                                                         <> "\n\nExamples of valid import formats are:"
                                                         <> "\n\timport * ..//myLookup.lookup"
                                                         <> "\n\timport (log_in book_up sign_out) ..//..//shared//myLookup.lookup"
                                                         <> "\n\timport * ..//shared.lookup hide (log_in book_up sign_out)\n\n"
                                            in
                                              badImportErr msg
                                            ) $
                                            do
                                              impTxt <- pathToIOEitherTxt fullImportPath
                                              (fullImportPath `List.elem` visited) ?

                                                badImportErr (pack $ "Cyclic dependencies are not allowed: "
                                                  <>  List.intercalate " -> " (List.dropWhile (/= fullImportPath) $
                                                  List.reverse $ fullImportPath : visited)) $

                                                case impTxt of
                                                  Left e -> pure $ Left e
                                                  Right txt -> do
                                                                imps <- parseImportedDefs fullImportPath (fullImportPath : visited) txt
                                                                pure $ refineLinkDefs fullImportPath imp =<< allImportLinks fullImportPath imps txt

reduceLinkDefs :: FilePath -> (Import -> IO (Either (FileParseError Char ErrorInfo) LinkDefinitions) ) -> [Import] -> IO (Either (FileParseError Char ErrorInfo) LinkDefinitions)
reduceLinkDefs callingFile resolver imprts =
                                              let
                                                mergeImpEith :: Either (FileParseError Char ErrorInfo) LinkDefinitions -> Either (FileParseError Char ErrorInfo) LinkDefinitions -> Either (FileParseError Char ErrorInfo) LinkDefinitions
                                                mergeImpEith accum item = do
                                                                            acm <- accum
                                                                            itm <- item
                                                                            let
                                                                              firstDupe = List.find (`Map.member` acm) (keys itm)

                                                                            maybe
                                                                              (Right $ Map.union acm itm)
                                                                              (\dupe -> Left $ IllegalImport ( pack callingFile <> ": more than one import contains the lookup: " <>
                                                                                                   dupe <> "\n"
                                                                               <> sourceFile (acm ! dupe) <> " and " <> sourceFile (itm ! dupe)))
                                                                              firstDupe

                                                reduceImps :: IO (Either (FileParseError Char ErrorInfo) LinkDefinitions) -> [Import] -> IO (Either (FileParseError Char ErrorInfo) LinkDefinitions)
                                                reduceImps rslt imps = case imps of
                                                                              [] -> rslt
                                                                              h : t -> let
                                                                                         thisImp = resolver h
                                                                                         reduced = mergeImpEith <$> rslt <*> thisImp
                                                                                       in
                                                                                        reduceImps reduced t
                                              in
                                                reduceImps (pure $ pure Map.empty) imprts

resolveImports :: FilePath -> [FilePath] -> [Import]-> IO (Either (FileParseError Char ErrorInfo) LinkDefinitions)
resolveImports callingFile visited imprts =  let
                                                resolver = resolveImport callingFile visited
                                              in
                                                reduceLinkDefs callingFile resolver imprts

parseImportedDefs :: FilePath -> [FilePath] -> Text -> IO (Either (FileParseError Char ErrorInfo) LinkDefinitions)
parseImportedDefs path visited content =
                                         let
                                            imprts = runParser parseImports path content
                                          in
                                            case imprts of
                                                Left e -> pure . Left $ ParserError e
                                                Right imps -> resolveImports path visited imps


innerLinks :: FilePath -> Either (FileParseError Char ErrorInfo) Text -> IO (Either (FileParseError Char ErrorInfo) LinkDefinitions)
innerLinks path fileText = do
                            let txt = fileText
                            either (pure . Left) (parseImportedDefs path [path] ) txt

parseQuickStepFile :: FilePath -> IO (Either (FileParseError Char ErrorInfo) ElementBlock)
parseQuickStepFile planPath = do
                                planText <- pathToIOEitherTxt planPath
                                links <- innerLinks planPath planText
                                pure $ do
                                        txt <- planText
                                        lks <- links
                                        mapLeft ParserError (parseQuickStepText planPath lks txt)

parseLookUp :: FilePath -> LinkDefinitions -> Text -> Either (ParseError Char ErrorInfo) LinkDefinitions
parseLookUp lookUpPath lnkDefs = runParser (evalStateT linkFileContent (linkedLinkState lnkDefs)) (fileLabel lookUpPath)

parseQuickStepText :: FilePath -> LinkDefinitions -> Text -> Either (ParseError Char ErrorInfo) ElementBlock
parseQuickStepText filePath lnkDefs = runParser (evalStateT linkedTextParser (linkedLinkState lnkDefs)) filePath

parseCopyTestPlan :: FilePath -> FilePath -> IO (Either (FileParseError Char ErrorInfo) FilePath)
parseCopyTestPlan fullPathTestPlan outputDir = parseCopy fullPathTestPlan outputDir testRunFileNameFromTestPlanPath parseExpand

testRunFilePathFromTestPlanPath :: FilePath -> FilePath
testRunFilePathFromTestPlanPath planPath = planPath P.-<.> runExtension

testRunFileNameFromTestPlanPath :: FilePath -> FilePath
testRunFileNameFromTestPlanPath planPath = takeFileName $ testRunFilePathFromTestPlanPath planPath

parseExpand :: FilePath -> IO (Either (FileParseError Char ErrorInfo) Text)
parseExpand fullPathTestPlan  = (expandBody <$>) <$> parseQuickStepFile fullPathTestPlan
