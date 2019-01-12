


{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SubParsers where

import           Control.Applicative  hiding (some)
import           Control.Monad
import           Data.Char
import           Data.List
import           Data.Monoid
import           Data.Text
import           LamdahofUtils        as Utils
import           Text.Megaparsec      as Megaparsec
import           Text.Megaparsec.Char hiding (eol)
-- Dirty needs factoring out mixing IO with parsing logic
import           Data.List.NonEmpty   as NE
import           Foundation.Extended  hiding (try)
import qualified Prelude              as P
import           System.FilePath

lookupExtension :: FilePath
lookupExtension = ".lookup"

planExtension :: P.String
planExtension = ".plan"

extensionIsLookup :: FilePath -> Bool
extensionIsLookup fileName = (Data.Char.toLower <$> takeExtension fileName) == lookupExtension


instance (ShowToken a, ShowErrorComponent b, Ord a) => Show (FileParseError a b) where
  show (ReadError ioErr) = P.show ioErr
  show (StatsReadFailure txt) = unpack txt
  show (IllegalImport txt) = unpack txt
  show (ParserError psErr) =
    let
       pos = NE.head $ errorPos psErr
       fileName = sourceName pos
       ext = (Data.Char.toLower <$> takeExtension fileName )
       prefix = extensionIsLookup fileName ?
                         "LookUp File Parser Error" $
                             ext == planExtension ?
                             "Test Plan File Parser Error" $
                                 "Parser Error"
       ttle = prefix ++ "\n" ++ fileName
    in
      ttle ++ "\n" ++
      "Line: " ++ P.show (sourceLine pos) ++ "\n" ++
      "Column: " ++ P.show (sourceColumn pos) ++ "\n" ++
      parseErrorTextPretty psErr


type Parser = Parsec ErrorInfo Text

data ErrorInfo = ErrorInfo [Text] (Maybe (ErrorFancy Text))
  deriving (Show, P.Read, Eq, Ord)


-- Dirty neds factoring out mixing IO with parsing logic
-- see pocket mock file system

data FileParseError a b =
  ReadError P.IOError |
  StatsReadFailure Text |
  IllegalImport Text |
  ParserError (ParseError a b)

pathToIOEitherTxt :: FilePath -> IO (Either (FileParseError Char ErrorInfo) Text)
pathToIOEitherTxt pth = convertEither <$> fileToText pth

convertEither :: Either P.IOError a -> Either (FileParseError Char ErrorInfo) a
convertEither = either (Left . ReadError) Right

-- end dirty

newtype ErrTxt = ErrTxt Text deriving (Eq, Ord, Show)

instance ShowErrorComponent ErrTxt where
  showErrorComponent :: ErrTxt -> P.String
  showErrorComponent (ErrTxt t) = unpack t

instance ShowErrorComponent ErrorInfo where
  showErrorComponent :: ErrorInfo -> P.String
  showErrorComponent (ErrorInfo errorStack maybeErrorFancy) =
                    let
                      formatLabels = Data.List.intercalate "\n" . fmap (unpack . ("in " <>) ) . Data.List.reverse
                      showError fancyErr = "" <> "\n" <> showErrorComponent (ErrTxt <$> fancyErr ) <> formatLabels errorStack
                    in
                      maybe "" showError maybeErrorFancy

colon :: Parser Char
colon = char ':'

fileLabel :: FilePath -> FilePath
fileLabel fullpath = takeFileName fullpath <> " - " <> fullpath

whiteSpace :: Parser Char
whiteSpace = oneOf whiteSpaceTokens

eolParser :: Bool -> Parser ()
eolParser consumeEol = consumeEol ? void eol $ void $ lookAhead eol

textOnMargin :: Parser ()
textOnMargin = satisfy isNotWhiteSpace >> pure ()

nonWhiteSpaceChar :: Parser Char
nonWhiteSpaceChar = satisfy isNotWhiteSpace

isEscapeToken :: Char -> Bool
isEscapeToken = (==) escapeToken

inLineWhiteSpace ::  Parser Text
inLineWhiteSpace = do
                        tors <- some tabOrSpace
                        pure $ pack tors

wordEndOrPunctuation :: Parser ()
wordEndOrPunctuation = try $ wordEnd <|> void punctuationChrExcludingUnderscore

nonWhiteSpaceNonPunctuationButForUnderscore :: Parser Char
nonWhiteSpaceNonPunctuationButForUnderscore = satisfy $ \c -> not (isPunctuationChrExcludingUnderscore c) && not (isWhiteSpace c)

escapeToken :: Char
escapeToken = '`'

wordEnd :: Parser ()
wordEnd = try $ eof <|> void punctuationExcludingUnderscore <|> void (oneOf whiteSpaceTokens)

punctuationExcludingUnderscore  :: Parser Text
punctuationExcludingUnderscore = (
                                    Data.Text.singleton <$> punctuationChrExcludingUnderscore
                                 ) <?> "special expected punctuation char"

plainChar :: Parser Char
plainChar = satisfy isPlainWordChar <?> "plain word char not an underscore or space"

isPunctuationChrExcludingUnderscore :: Char -> Bool
isPunctuationChrExcludingUnderscore c = not (isUnderscore c)
                                          && isPunctuationChr c

isPunctuationChr :: Char -> Bool
isPunctuationChr c = not (isDigit c)
                     && (isPunctuation c || isSymbol c)

punctuationChrExcludingUnderscore :: Parser Char
punctuationChrExcludingUnderscore = satisfy isPunctuationChrExcludingUnderscore

whiteSpaceTokens :: P.String
whiteSpaceTokens = " \n\r\t"

isWhiteSpace :: Char -> Bool
isWhiteSpace = hasA whiteSpaceTokens

isNotWhiteSpace :: Char -> Bool
isNotWhiteSpace ch = not $ isWhiteSpace ch

isPlainWordChar :: Char -> Bool
isPlainWordChar ch = isPlainWordCharOrUnderScore ch
                        && isNotUnderscore ch

isPlainWordCharOrUnderScore :: Char -> Bool
isPlainWordCharOrUnderScore ch = isAscii ch
                                    && not (isPunctuationChrExcludingUnderscore ch)
                                    && isNotWhiteSpace ch


eol :: Parser Text
eol =   try (string "\n\r") <|>
                   try (string "\r\n") <|>
                   try (string "\r")  <|>
                   string "\n"
                    <?> "end of line"

someLines :: Parser [Text]
someLines = some eol

--Maybe make this configuarable later
tabOrDoubleSpace :: Parser Text

tabOrDoubleSpace = string "\t" <|> string "  "

tabOrSpace :: Parser Char
tabOrSpace = oneOf [' ',  '\t']

whiteSpaceLine :: Parser Text
whiteSpaceLine = pack <$> manyTill tabOrSpace (eof <|> void eol)

underscore :: Parser Char
underscore = char underscoreToken

underscoreToken :: Char
underscoreToken = '_'

isUnderscore :: Char -> Bool
isUnderscore = (==) underscoreToken

isNotUnderscore :: Char -> Bool
isNotUnderscore ch = not $ isUnderscore ch
