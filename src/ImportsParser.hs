{-# LANGUAGE OverloadedStrings   #-}
-- {-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE LambdaCase #-}


module ImportsParser where

import           Control.Monad
import           Data.List            as List
import           Data.Maybe
import           Data.Monoid
import           Data.Set             as Set
import           Data.Text            as Text
import           SubParsers
import           Text.Megaparsec      hiding (hidden)
import           Text.Megaparsec.Char hiding (eol)


data Import = Import {
  relativePath :: FilePath,
  imports      :: ImportRecord
} deriving (Eq, Ord, Show)

data ImportRecord = Wildcard {hide :: Set Text} |
                    Explicit {includes :: Set Text}
                  deriving (Eq, Ord, Show)

parseImports :: Parser [Import]
parseImports = do
                _ <- many eol
                many parseImport

mustBeLookup :: Parser Import -> Parser Import
mustBeLookup imp = do
                      i <- imp
                      unless (extensionIsLookup (relativePath i))
                        $ fail $ "import extension must be " <> lookupExtension <> " invalid import path: " <> relativePath i
                      pure i

validateImport ::  Parser Import -> Parser Import
validateImport base = List.foldl (\p v -> v p) base [mustBeLookup]

parseImport :: Parser Import
parseImport = do
                _ <- string "import"
                _ <- whiteSpaces
                validateImport $ try wildcard <|> explicit

trimStr :: String -> String
trimStr = unpack . strip . pack

filePathPsr :: Parser String
filePathPsr = manyTill anyChar (void eol <|> void (lookAhead hidden) <|> eof)

wildcard :: Parser Import
wildcard = do
             _ <- char '*'
             _ <- whiteSpaces
             filePath <- filePathPsr
             _ <- many whiteSpace
             hides <- optional hidden
             pure Import {
              relativePath = trimStr filePath,
              imports = Wildcard $ fromMaybe Set.empty hides
             }

hiddenStart :: Parser Text
hiddenStart = do
               _ <- string "hide" >> some whiteSpace >> char '('
               pure ""

hidden :: Parser (Set Text)
hidden = string "hide" >> some whiteSpace >> bracketVals

bracketVals :: Parser (Set Text)
bracketVals = between (char '(') (char ')') spacedItems

word :: Parser String
word = some $ noneOf $ whiteSpaceTokens <> "()"

whiteSpaces :: Parser String
whiteSpaces = some whiteSpace

spacedItems :: Parser (Set Text)
spacedItems =  do
                _ <- many whiteSpace
                items <- sepBy word whiteSpaces
                pure $ fromList $ pack <$> items

explicit :: Parser Import
explicit = do
             items <- bracketVals
             _ <- whiteSpaces
             filePath <- filePathPsr
             pure Import {
              relativePath = trimStr filePath,
              imports = Explicit items
             }
