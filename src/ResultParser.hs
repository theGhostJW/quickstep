{-# LANGUAGE InstanceSigs        #-}
-- {-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE LambdaCase #-}

module ResultParser where

import           Data.Text  hiding (count)
import           SubParsers
import           ResultParserShared
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Control.Monad


type ResultParser = Parser

rawTestCases :: Parser [RawTestResult]
rawTestCases = do
                _ <- preambleOrContentText
                many rawTestCase

rawTestCase :: Parser RawTestResult
rawTestCase = do
                ttle <- testTitle
                text <- preambleOrContentText
                pure TestResult {
                  title = ttle,
                  content = pack text
                }


countChars :: Int -> Parser a -> Parser [a]
countChars = count

sectionBoundary :: Char -> Parser Text
sectionBoundary ch = try $ pack <$> countChars 4 (char ch)

inHeader :: Char ->  Parser a -> Parser a
inHeader ch psr = do
                r <- between (sectionBoundary ch) (sectionBoundary ch) psr
                _ <- many tabOrSpace
                pure r

header :: Char -> String ->  Parser Text
header ch errorMsg = inHeader ch
                                 (
                                   do
                                      str <- many anyChar
                                      return $ strip $ pack str
                                 )
                                 <?> errorMsg

titleEdge :: Parser Text
titleEdge = sectionBoundary '-'

notEolChar :: Parser (Token Text)
notEolChar = satisfy (\c -> (c /= '\n') && c /= '\r')

testTitle :: Parser Text
testTitle = do
              _ <- titleEdge
              tstTitle <- manyTill notEolChar titleEdge
              pure $ strip $ pack tstTitle

preambleOrContentText :: Parser String
preambleOrContentText = manyTill anyChar (eof <|> void (try (lookAhead testTitle)))
