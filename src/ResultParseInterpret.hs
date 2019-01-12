{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE LambdaCase #-}

module ResultParseInterpret where

import           SubParsers
import           ResultParser
import           ResultInterpreter
import           Text.Megaparsec

parseInterpretResult :: FilePath -> IO (Either (FileParseError Char ErrorInfo) FullInfo)
parseInterpretResult resultPath  = do
                                      resultText <- pathToIOEitherTxt resultPath
                                      let
                                        result = do
                                          txt <- resultText
                                          either (Left . ParserError)
                                                 Right (runParser rawTestCases resultPath txt)
                                      pure $ testStats <$> result
