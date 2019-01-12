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
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module MergeEngine where

import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import           Data.Foldable
import           Data.List
import           Data.Monoid
import           Data.String
import           Data.Text
import           Data.Time
import           Data.Typeable
import           Foundation.Extended  as F
import           LamdahofUtils
import           ParserRunner
import qualified Prelude              as P
import           ReportGenerator
import           ResultParseInterpret
import           SubParsers
import           System.Directory     as D
import           System.Environment   as E
import           System.FilePath      as P
import           System.FilePath.Find
import           System.IO
import           TestPlanParser

main :: IO ()
main = do
        rawArgs <- E.getArgs
        args <- parseValidateArgs rawArgs
        either
            (print  . show)
            run args

setUpLog :: FilePath -> FilePath -> IO()
setUpLog src dest = do
                      System.IO.putStrLn $ "Monitoring Directory " <> src <> " writing to " <> dest
                      createDirectoryIfMissing False dest

-- :main "C:\\Automation\\QuickStep\\Parser\\testData" "C:\\Automation\\QuickStep\\Parser\\temp"
run :: RunArgs ->  IO ()
run runArgs = do
                setUpLog (plansDir runArgs) (previewDir runArgs)
                setUpLog (runsDir runArgs) (statsDir runArgs)
                setUpLog (statsDir runArgs) (reportsDir runArgs)
                 -- pretty up with file watcher one day
                recursiveCycle (UTCTime (fromGregorian 1900 1 1) $ secondsToDiffTime 0 ) runArgs

cycleFileOp :: FilePath -> FilePath -> FilePath -> FileOperator -> IO()
cycleFileOp sourceExt srcDir dstDir loaderFunc = do
                              deleteUnusedTmpDirs srcDir dstDir
                              insertMissingDirectories srcDir dstDir
                              loaderFunc srcDir dstDir
                              deleteUnmatchedDestinationFiles sourceExt srcDir dstDir


lookUpChanged :: UTCTime -> UTCTime -> FilePath -> Bool
lookUpChanged since fileTime path = extensionIsLookup path && (since < fileTime)

-- getFlags ::  UTCTime -> IO [FilePath] -> IO [Bool]
-- good example how to do this (UTCTime -> FilePath -> IO Bool)  -> IO [FilePath] -> IO [Bool]

-- lstModified :: FilePath -> IO UTCTime
findFirstFile :: IO [FilePath] -> (FilePath -> IO Bool) -> IO Bool
findFirstFile paths predicate = do
                            pths <- paths
                            case pths of
                              [] -> pure False
                              x : xs -> do
                                         match <- predicate x
                                         match ? pure True $ findFirstFile (pure xs) predicate

anyLkChanged :: UTCTime -> IO [FilePath] -> IO Bool
anyLkChanged since paths =  let
                              relevantChange :: FilePath -> IO Bool
                              relevantChange ph = do
                                                      modTime <- lstModified ph
                                                      pure $ lookUpChanged since modTime ph
                              in
                               findFirstFile paths relevantChange

anyLookupChanged :: FilePath -> UTCTime -> IO Bool
anyLookupChanged dir fromTime = anyLkChanged fromTime (findFilesRecursive dir always)

recursiveCycle :: UTCTime -> RunArgs ->  IO ()
recursiveCycle time runArgs = do
                          let plansDirectory = plansDir runArgs
                          lastRun <- getCurrentTime
                          changed <- anyLookupChanged plansDirectory time
                          let force = changed ? RunAlways $ IfChanged
                          cycleFileOp planExtension plansDirectory (previewDir runArgs) (generateTestRuns force)
                          cycleFileOp runExtension (runsDir runArgs) (statsDir runArgs) (\src dest -> visitAllFullRecursion src dest isRunFile parseResultSaveStats)
                          cycleFileOp statsExtension (statsDir runArgs) (reportsDir runArgs) (\src dest -> visitAllFullRecursion src dest isStatsFile parseStasSaveReport)
                          threadDelay 500000
                          recursiveCycle lastRun runArgs

type FileOperator = (FilePath -> FilePath -> IO())

type FileFinder = FilePath -> FindClause Bool -> IO [FilePath]

insertDirIfMissingFromDest :: FileOperator
insertDirIfMissingFromDest srcPath tmpPath = do
                                              exists <- doesDirectoryExist tmpPath
                                              unless exists $
                                                consoleGreen $ print $ "Creating Directory: " <> tmpPath
                                              createDirectoryIfMissing False tmpPath
                                              visitAllDirs srcPath tmpPath insertDirIfMissingFromDest

insertMissingDirectories :: FilePath -> FilePath -> IO()
insertMissingDirectories src dest = visitAllDirs src dest insertDirIfMissingFromDest

-- _insertMissingDirectories = insertMissingDirectories  ApArgs {
--                                               sourceDir = "C:\\Automation\\QuickStep\\Parser\\Temp",
--                                               destDir = "C:\\Automation\\QuickStep\\Parser\\PreviewDir"
--                                             }

deleteDirIfMissingFromSource :: FileOperator
deleteDirIfMissingFromSource srcPath tmpPath  = do
                                            exists <- doesDirectoryExist srcPath
                                            exists ?
                                              visitAllDirs tmpPath srcPath deleteDirIfMissingFromSource $
                                              do
                                                print $ "Deleting Directory: " <> tmpPath
                                                removeDirectoryRecursive tmpPath

deleteUnusedTmpDirs :: FilePath -> FilePath -> IO()
deleteUnusedTmpDirs src dest = visitAllDirs src dest deleteDirIfMissingFromSource


visitAllDirs :: FilePath -> FilePath -> FileOperator -> IO()
visitAllDirs inputDir targetDir = visitAll inputDir targetDir isDirectoryFindClause

isFileWithExt:: FilePath -> FindClause Bool
isFileWithExt ext = fileName ~~? ("*" <> ext)

isLookupFile :: FindClause Bool
isLookupFile = isFileWithExt lookupExtension

isRunFile :: FindClause Bool
isRunFile = isFileWithExt runExtension

isTestFile :: FindClause Bool
isTestFile = isFileWithExt planExtension

isStatsFile :: FilterPredicate
isStatsFile = isFileWithExt statsExtension

sourceToTempPath :: FilePath -> FilePath -> FilePath -> FilePath
sourceToTempPath sourceExt runPath planPath = combine (takeDirectory planPath) $ takeFileName runPath P.-<.> sourceExt

deleteTempIfSourceMissing :: FilePath -> FilePath -> FilePath -> IO()
deleteTempIfSourceMissing sourceEx tmpFile sourcefile = do
                                                  let
                                                    planPath = sourceToTempPath sourceEx tmpFile sourcefile
                                                  planExists <- D.doesFileExist planPath
                                                  unless planExists $
                                                    do
                                                      print $ planPath <> " does not exist so deleteing file " <> tmpFile
                                                      D.removeFile tmpFile

isRegularNonGitIgnoreFile :: FilterPredicate
isRegularNonGitIgnoreFile = isRegularFile &&? not <$> matchesPattern ".gitignore"

deleteUnmatchedDestinationFiles :: FilePath -> FilePath -> FilePath -> IO()
deleteUnmatchedDestinationFiles sourcExt srcDir destDr =  ---- Note swithching dest and Source as dest is the source for deletion
                                                  visitAllFullRecursion destDr srcDir isRegularNonGitIgnoreFile (deleteTempIfSourceMissing sourcExt)


lstModified :: FilePath -> IO UTCTime
lstModified path =
    do
      exists <- D.doesFileExist path
      exists ? D.getModificationTime path $ pure UTCTime {
                                                           utctDay = fromGregorian 1970 1 1,
                                                           utctDayTime = 0
                                                         }

singleFileOperator :: (FilePath -> IO (Either (FileParseError Char ErrorInfo) a)) -> FilePath -> (a -> Text) -> FileOperator
singleFileOperator fileParser destExt prettyPrinter sourcePath destPath =
                                      let
                                        trueDestPath = destPath P.-<.> destExt
                                        failLog = logReturnFailure trueDestPath
                                        runStats = void $ do
                                                             info <- fileParser sourcePath
                                                             either failLog (successSave trueDestPath . prettyPrinter) info
                                      in
                                        do
                                          wantGenerate <- (>) <$> lstModified sourcePath <*> lstModified trueDestPath
                                          when
                                            wantGenerate runStats


parseResultSaveStats :: FileOperator
parseResultSaveStats = singleFileOperator parseInterpretResult statsExtension (pack . P.show)

parseStasSaveReport :: FileOperator
parseStasSaveReport = singleFileOperator parseStatsToReport reportExtension id

data Force = RunAlways | IfChanged deriving (Eq)

parseFile :: Force -> FileOperator
parseFile force planPath destFile =
                      let
                        runPath = testRunFilePathFromTestPlanPath destFile
                        runParseCopy = void $
                                         parseCopyTestPlan planPath $ takeDirectory destFile
                        needGen planMod = (planMod >)
                      in
                        do
                          wantGenerate <- needGen <$> lstModified planPath
                                                  <*> lstModified runPath
                          when (force == RunAlways || wantGenerate)
                              runParseCopy

generateTestRuns :: Force -> FileOperator
generateTestRuns force src dest  = visitAllFullRecursion src dest isTestFile $ parseFile force

--_generateTestRuns = generateTestRuns $ ApArgs "C:\\Automation\\QuickStep\\Parser\\testData" "C:\\ManualTests" "C:\\ManualTests"

visitAllCustom :: FileFinder -> FilePath -> FilePath -> FindClause Bool -> FileOperator -> IO()
visitAllCustom  finderFunction inputDir targetDir finderClause visitor = do
                                              targets <- finderFunction inputDir finderClause
                                              let pathVisitor = executeOperator inputDir targetDir visitor
                                              traverse_ pathVisitor $ delete inputDir targets

visitAllFullRecursion :: FilePath -> FilePath -> FindClause Bool -> FileOperator -> IO ()
visitAllFullRecursion = visitAllCustom findFilesRecursive

visitAll :: FilePath -> FilePath -> FindClause Bool -> FileOperator -> IO()
visitAll = visitAllCustom findFilesNonRecursive

printIt :: (Data.String.IsString a, Monoid a, Show a) => a -> a -> IO ()
printIt txt txt2 = print $ txt <> " -> " <> txt2

executeOperator :: FilePath -> FilePath -> FileOperator -> FilePath -> IO ()
executeOperator inputDir targetDir operator inputPath =
                                                      let
                                                        childPath = P.makeRelative inputDir inputPath
                                                        destPath = combine targetDir childPath
                                                      in
                                                        operator inputPath destPath

data ApArgs = ApArgs {
                             sourceDir   :: FilePath,
                             destDir     :: FilePath,
                             testRunsDir :: FilePath
                          }
                          deriving (Eq, Show)

data RunArgs = RunArgs {
                       plansDir   :: FilePath,
                       runsDir    :: FilePath,
                       previewDir :: FilePath,
                       statsDir   :: FilePath,
                       reportsDir :: FilePath
                    }
                    deriving (Eq, Show)

newtype CommandLineError = CommandLineError Text
                          deriving (Typeable, Show)

instance Exception CommandLineError

convertArgs :: ApArgs -> IO (Either CommandLineError RunArgs)
convertArgs = pure . Right . apToRunArgs

parseValidateArgs :: [P.String] -> IO (Either CommandLineError RunArgs)
parseValidateArgs args = do
                          let eithApArgs = parseArgsPure args
                          either
                            (pure . Left)
                            convertArgs
                            eithApArgs


apToRunArgs :: ApArgs -> RunArgs
apToRunArgs aa =
            let
              destChild = combine (destDir aa)
            in
              RunArgs {
                  plansDir = sourceDir aa,
                  runsDir = testRunsDir aa,
                  previewDir = destChild "Preview",
                  statsDir = destChild "Stats",
                  reportsDir  = destChild "Reports"
                }

validateArgs :: ApArgs -> IO (Either CommandLineError ApArgs)
validateArgs args = do
                      srcExists <- doesDirectoryExist $ sourceDir args
                      destExists <- doesDirectoryExist $ destDir args
                      return  $ srcExists && destExists ?
                                              Right args $
                                              Left $ CommandLineError ("either source or dest directory does not exist" <> pack (P.show args))


parseArgsPure :: [P.String] -> Either CommandLineError ApArgs
parseArgsPure args = P.length args == 3 ? Right (ApArgs (P.head args) (args !! 1) (args !! 2)) $
                            Left $ CommandLineError "Incorrect number of command line parameters expect 3 arguments: source directory, parent of Preview directory, parent of TestRuns directory,"
