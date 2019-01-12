{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE LambdaCase #-}

module ParserRunner where

import           Data.Monoid
import           Data.Text
import           Data.Text.IO
import           Foundation.Extended (Truthy (..))
import           LamdahofUtils       as Utils
import           SubParsers
import           System.Directory
import           System.FilePath
import           System.IO.Error



parseCopy :: FilePath -> FilePath -> (FilePath -> FilePath) -> (FilePath -> IO (Either (FileParseError Char ErrorInfo) Text)) -> IO (Either (FileParseError Char ErrorInfo) FilePath)
parseCopy fullPathToSource outputDir outputFileNameCalc parseRunner = do
                                        let outputFilePath = combine outputDir $ outputFileNameCalc fullPathToSource
                                            failLog = logReturnFailure outputFilePath
                                        dirValid <- doesDirectoryExist outputDir
                                        dirValid ?
                                                  do
                                                    txtEth <- parseRunner fullPathToSource
                                                    either failLog (successSave outputFilePath) txtEth
                                                  $
                                                   do
                                                     let message = "invalid directory - directory does not exist: " <> outputDir
                                                     print message
                                                     failLog $ ReadError $ mkIOError
                                                                             doesNotExistErrorType
                                                                             message
                                                                             Nothing
                                                                             (Just outputDir)


runExtension :: FilePath
runExtension = ".rslt"

statsExtension :: FilePath
statsExtension = ".stats"

reportExtension :: FilePath
reportExtension = ".html"

planExtensionTxt :: Text
planExtensionTxt = pack planExtension

logReturnFailure :: (Show e, Show a) => FilePath -> e ->  IO (Either e a)
logReturnFailure destPath err = consoleRed (
                                           do
                                             let errTxt = pack $ show err
                                             sequence_ $ Data.Text.IO.putStrLn <$> Data.Text.lines errTxt
                                             _ <- saveText destPath errTxt
                                             return $ Left err
                                            )

successSave :: FilePath -> Text -> IO (Either (FileParseError Char ErrorInfo) FilePath)
successSave destPath contentTxt = consoleGreen $ saveText destPath contentTxt

saveText :: FilePath -> Text -> IO (Either (FileParseError Char ErrorInfo) FilePath)
saveText destPath contentTxt = do
                             Prelude.putStrLn ("Writing file to: " <> destPath)
                             Data.Text.IO.writeFile destPath contentTxt
                             return $ Right destPath
