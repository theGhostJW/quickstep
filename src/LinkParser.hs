{-# LANGUAGE OverloadedStrings   #-}
-- {-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}


module LinkParser where

import           Control.Applicative  hiding (many, some)
import           Control.Monad
import           Control.Monad.State
import           Data.Either
import           Data.Functor
import           Data.List            as List
import           Data.List.NonEmpty   as NonEmpty
import           Data.Map.Strict
import qualified Data.Map.Strict      as Map
import           Data.Maybe
import           Data.Monoid
import           Data.Set             as Set
import           Data.Text            as Text
import           Foundation.Extended  hiding (try)
import           ImportsParser
import           LamdahofUtils        as Utils
import           LinkParserTypes
import           Prelude              (Show (..))
import qualified Prelude              as P
import           SubParsers
import           Text.Megaparsec
import           Text.Megaparsec.Char hiding (eol)

type LinksParser = StateT LinkState Parser

convertErrorItem :: ErrorItem Text -> ErrorFancy Text
convertErrorItem = \case
                  Tokens l  -> ErrorCustom $ Text.intercalate "\n" $ NonEmpty.toList l
                  Label chrs  -> ErrorCustom $ pack $ NonEmpty.toList chrs
                  EndOfInput -> ErrorCustom "End Of Input"

addConvertMaybeItem :: ErrorFancy Text -> Maybe (ErrorItem Text) -> Set (ErrorFancy Text)
addConvertMaybeItem ef me = Set.fromList $ Data.Maybe.catMaybes [Just ef, convertErrorItem <$> me]

-- to do put label back
attachLabel :: Text -> LinksParser t -> LinksParser t
attachLabel lbl psr = do
                        result <- observing psr
                        let
                          lblStr = unpack lbl
                          err :: ErrorItem Char
                          err = Label $ NonEmpty.fromList lblStr

                        case result of
                                      Left (TrivialError _ mUnexpected expected) ->
                                        failure mUnexpected $ Set.insert err expected

                                      Left (FancyError _ fancyItems) ->
                                        fancyFailure $ Set.insert (ErrorFail lblStr) fancyItems

                                      Right x -> pure x

(<??>) :: LinksParser t -> Text -> LinksParser t
(<??>) = flip attachLabel

linkedWordsSpecial :: [Text] -> Bool -> LinksParser [BodyElement]
linkedWordsSpecial paramRefs consumeEol =
                        let
                           -- try choice does not work here consumes input maybe?
                           bodyElements :: LinksParser BodyElement
                           bodyElements  =  try eolElement
                                               <|> try escapedUnderScore
                                               <|> try stepTypeElement
                                               <|> try spaceElement
                                               <|> try tabElement
                                               <|> try (makeLit inLineWhiteSpace)
                                               <|> try (makeLit punctuationExcludingUnderscore)
                                               <|> try (Ref <$> linkRef paramRefs)
                                               <|> try escapedLink
                                               <|> try plainWordStr
                                               <|> inScopeLinkParamRef paramRefs
                         in
                           manyTill
                              (toggleAllEscapes >> bodyElements)
                              (try (toggleAllEscapes >> eof) <|> try (toggleAllEscapes >> lift (eolParser consumeEol)))


strToStepType :: Text -> Maybe StepType
strToStepType = \case
                  "#"  -> Just Instruction
                  "=>" -> Just Validation
                  _    -> Nothing

stepType :: Parser StepType
stepType = do
              tkn <- string "#" <|> string "=>"
              let stepTypeM = strToStepType tkn
              when (isNothing stepTypeM)
                $ fail "Invalid step type token"
              return $ fromJust stepTypeM

initialLinkState :: LinkState
initialLinkState = LinkState {
   escapeStatus = Unescaped,
   escapes = Set.empty,
   linkDefs = Map.empty
}

linkedLinkState :: LinkDefinitions -> LinkState
linkedLinkState lDefs = initialLinkState {linkDefs = lDefs}

linkFileContent :: LinksParser LinkDefinitions
linkFileContent = do
                    _ <- lift parseImports
                    defs <- many linkDefinition
                    pure $ fromLinkDefArray defs

linkDefinition :: LinksParser LinkDefinition
linkDefinition = linkDefinitionPrimative newLineAndLink

linkDefinitionPrimative :: Parser () -> LinksParser LinkDefinition
linkDefinitionPrimative terminator = do
                                      declare <- linkDeclaration
                                      pos <- getPosition
                                      _ <- lift eol
                                      bdy <- linkedBody (linkName declare) (params declare) (try terminator <|> eof)
                                      let linkDef = LinkDefinition {
                                                                      sourceFile = pack $ sourceName pos,
                                                                      declaration = declare,
                                                                      body = bdy
                                                                    }
                                      updateLinks (linkName declare) linkDef
                                      pure linkDef


readLinkDefToState :: LinksParser ()
readLinkDefToState = linkDefinitionPrimative (whiteSpaceLine <|> eol >> lookAhead textOnMargin) $> ()

getLinks :: LinksParser LinkDefinitions
getLinks = gets linkDefs

getEscapes :: LinksParser (Set Text)
getEscapes = gets escapes

updateLinks :: Text -> LinkDefinition -> LinksParser ()
updateLinks lnkName linkDef = do
                                links <- gets linkDefs
                                Map.member lnkName links ?
                                    fail ("Duplicate key declaration: " <> unpack lnkName) $
                                    modify $ \s -> s { linkDefs = Map.insert lnkName linkDef links}
                                pure ()

fromLinkDefArray ::  [LinkDefinition] -> LinkDefinitions
fromLinkDefArray defs = Map.fromList $ (\ld -> (linkName  (declaration ld), ld)) <$> defs

linkDeclaration :: LinksParser LinkDeclaration
linkDeclaration = do
                     pos  <- getPosition
                     name <- readLinkNameInDeclaration
                     _ <- many $ lift tabOrSpace
                     prms <- lift linkParamRefs
                     pure LinkDeclaration {
                                              linkName = name,
                                              params = prms,
                                              linkPos = pos
                                            }

linkParamRef :: Parser Text
linkParamRef = (
             do
               _ <- underscore
               content <- some plainChar
               lookAhead wordEnd
               pure $ pack content
             ) <?> "start with underscore and no underscores in param"

consumeToggleBlockEsc :: LinksParser ()
consumeToggleBlockEsc = try
               (
                do
                   escStatus <- gets escapeStatus
                   unless (escStatus == SingleWord) $
                      do
                         _ <- Text.Megaparsec.count 3 (char '\"')
                         modify $ \s -> s { escapeStatus = escapeStatus s == MultiWord ? Unescaped $ MultiWord}
                )
               <|>
                pure ()

toggleSingleEscaped :: LinksParser ()
toggleSingleEscaped = do
                        escStatus <- gets escapeStatus
                        try
                          (
                            do -- Toggle off
                              _ <- lift $ lookAhead whiteSpace
                              when (escStatus == SingleWord)  $
                                modify $ \s -> s {escapeStatus = Unescaped}
                            )
                          <|>
                            when (escStatus == Unescaped) (
                                do
                                _ <- lift $ char escapeToken
                                modify $ \s -> s {escapeStatus = SingleWord}
                            )
                          <|>
                            pure ()

escapedWord :: Parser P.String
escapedWord = some nonWhiteSpaceNonPunctuationButForUnderscore

escapedLink :: LinksParser BodyElement
escapedLink =  Lit <$> do
                        escaps <- gets escapes
                        when (Set.null escaps) $
                          fail "null escapes"
                        readLinkNameInInvocation True <??> "escaped link"

plainWordStr :: LinksParser BodyElement
plainWordStr =  Lit <$>
                        do
                          escStatus <- gets escapeStatus
                          content <- lift $ (escStatus == Unescaped) ? some plainChar $ escapedWord
                          lookAhead $ lift wordEndOrPunctuation
                          let rslt = pack content
                          when (escStatus == SingleWord) $
                            modify $ \s -> s {escapes = Set.insert rslt (escapes s)}
                          pure rslt
                        <??> "plain word string"

linkParamRefs :: Parser [Text]
linkParamRefs = sepEndBy linkParamRef $ some tabOrSpace

data LinkContext = Declaration | Invocation deriving (Eq, Show)
data Indented = Indented | Unindented deriving (Eq, Show)

escaped :: LinksParser Bool
escaped = do
            escapStatus <- gets escapeStatus
            pure $ escapStatus /= Unescaped


readLinkName :: (Char -> P.String -> Text -> LinksParser ())-> LinksParser Text
readLinkName keyValidation = do
                                isEscaped <- escaped
                                when isEscaped $
                                  fail "links cannot be defined in escaped text"

                                firstChar <- noneOf $ [underscoreToken,  escapeToken] <> whiteSpaceTokens
                                rest <- lift escapedWord
                                lookAhead $ lift wordEndOrPunctuation
                                let
                                  resultStr = firstChar : rest
                                  rsltTxt = pack resultStr

                                linkNameTextValidation firstChar resultStr
                                keyValidation firstChar resultStr rsltTxt
                                pure rsltTxt


readLinkNameInDeclaration :: LinksParser Text
readLinkNameInDeclaration =
                        let
                          keyValidation :: Char -> P.String -> Text -> LinksParser ()
                          keyValidation firstChar key keyTxt = do
                                                      links <- getLinks
                                                      when (Map.member keyTxt links)
                                                        $ fail $ "Link re-declared (ie. this link name has already ben used): " <> key
                        in
                          do
                            -- consume leading underscore
                            lift $ void underscore
                            readLinkName keyValidation


readLinkNameInInvocation ::  Bool -> LinksParser Text
readLinkNameInInvocation expectEscaped = let
                                            keyValidation :: Char -> P.String -> Text -> LinksParser ()
                                            keyValidation firstChar key keyTxt = do
                                                                                    links <- getLinks
                                                                                    escapedSet <- gets escapes
                                                                                    let linkIsEscaped = Set.member keyTxt escapedSet && expectEscaped
                                                                                    unless (not expectEscaped && Map.member keyTxt links || linkIsEscaped)
                                                                                        $ fail $ "Using undeclared link name in definition: " <> key
                                            in
                                             readLinkName keyValidation


linkNameTextValidation :: Char -> P.String -> LinksParser ()
linkNameTextValidation firstChar lnkName = do
                                            unless ( '_'  `List.elem` lnkName)
                                                $ fail $ "no underscore in link " <> lnkName

                                            when (isPunctuationChr firstChar) $
                                                fail "links cannot have punctuation immediately after leading underscore"

eols :: LinksParser [BodyElement]
eols =  do
         lns <- lift someLines
         pure $ Lit <$> lns



isLinkDefPlaceHolder  ::  [BodyElement] -> Bool
isLinkDefPlaceHolder elements =
                            let
                               isPlaceHolderElement ::  BodyElement -> Bool
                               isPlaceHolderElement el =
                                  case el of
                                   NewLine               -> True
                                   LinkParserTypes.Space -> True
                                   Tab                   -> True
                                   StepToken _           -> True
                                   Lit "?"               -> True
                                   Lit ""                -> True
                                   _                     -> False
                            in
                              List.all isPlaceHolderElement elements


linkedBody :: Text -> [Text] -> Parser () -> LinksParser [BodyElement]
linkedBody lnkName paramRefs delimParser = do
                                 bod  <- bodyLines Indented paramRefs delimParser
                                 pure $ isLinkDefPlaceHolder bod ? [Lit $ "??" <> lnkName <> "??"]  $ bod

linkedTextParser :: LinksParser [BodyElement]
linkedTextParser  = do
                      _ <- lift parseImports
                      bodyLines Unindented [] eof

linkedTextBodyLine :: Indented -> [Text] -> LinksParser [BodyElement]
linkedTextBodyLine indented paramRefs = do
                                          _ <- many readLinkDefToState
                                          when (indented == Indented)
                                                (void $ lift tabOrDoubleSpace)
                                          linkedWords paramRefs

singleEmptyLine :: LinksParser [BodyElement]
singleEmptyLine = do
                     whiteSpc <- makeLit whiteSpaceLine
                     pure [whiteSpc]

bodyLines :: Indented ->  [Text] -> Parser () -> LinksParser [BodyElement]
bodyLines indented paramRefs delimParser  =  do
                                 bodNested <- manyTill (linkedTextBodyLine indented paramRefs <|> singleEmptyLine)
                                                        (lift delimParser)
                                                         <??> "expecting an empty line or <Tab> or <Double Space> then text"
                                 pure $ List.intercalate [NewLine] bodNested

newLineAndLink :: Parser ()
newLineAndLink = newLineThen linkDeclaration

newLineAndMarginText :: Parser ()
newLineAndMarginText = newLineThen $ lift textOnMargin

newLineThen :: LinksParser a ->  Parser ()
newLineThen psr = do
                    _ <- lookAhead $ whiteSpaceLine >>
                                runStateT psr initialLinkState
                    _ <- many whiteSpaceLine
                    pure ()

linkedWords :: [Text]  -> LinksParser [BodyElement]
linkedWords paramRefs = linkedWordsSpecial paramRefs True

makeLit :: Parser Text -> LinksParser BodyElement
makeLit psr = do
               txt <- lift psr
               pure $ Lit txt

escapedUnderScore :: LinksParser BodyElement
escapedUnderScore = do
                     isEscaped <- escaped
                     unless isEscaped $
                        fail "not escaped"
                     content <- Text.singleton <$> char '_'
                     pure $ Lit content


eolElement :: LinksParser BodyElement
eolElement = do
               _ <- lift eol
               pure NewLine

spaceElement :: LinksParser BodyElement
spaceElement = do
                _ <- lift $ char ' '
                pure LinkParserTypes.Space

tabElement :: LinksParser BodyElement
tabElement = do
              _ <- lift tabOrDoubleSpace
              pure Tab

stepTypeElement :: LinksParser BodyElement
stepTypeElement = do
                   st <- lift stepType
                   pure $ StepToken st

toggleAllEscapes :: LinksParser ()
toggleAllEscapes = consumeToggleBlockEsc >> toggleSingleEscaped

inScopeLinkParamRef :: [Text] -> LinksParser BodyElement
inScopeLinkParamRef prms =
                              lift $ do
                                     linkPrm <- linkParamRef
                                     unless (linkPrm `List.elem` prms) $
                                        fail $ unpack $ "link param: " <> linkPrm
                                                <> " is not declared in link declaration - link params are: "
                                                <> Text.unlines prms
                                     pure $ LinkArg linkPrm

linkRef :: [Text] -> LinksParser LinkRef
linkRef parentLinkParams = do
                              inEscape <- escaped
                              when inEscape $
                                 fail "no link refs when escaped"

                              lnkName <- readLinkNameInInvocation False

                              allLinks <- getLinks
                              let linkDef = allLinks ! lnkName
                                  paramRefs = params $ declaration linkDef
                              args <- linkArgsRead lnkName paramRefs parentLinkParams
                              pure $ LinkRef linkDef args

{-# ANN linkArgsRead ("HLint: ignore Eta reduce" :: P.String) #-}
linkArgsRead :: Text -> [Text] -> [Text] -> LinksParser ParamMap
linkArgsRead lnkName allArgs parentLinkParams = linkArgsPriv lnkName allArgs allArgs Map.empty parentLinkParams

linkArgsPriv :: Text -> [Text]  -> [Text] -> ParamMap -> [Text] -> LinksParser ParamMap
linkArgsPriv lnkName allArgs unassignedArgs assignedArgs parentLinkParams = (
                                      if List.null unassignedArgs then
                                          pure assignedArgs
                                      else
                                        do
                                          _ <- lift $ some whiteSpace
                                          (updatedUnassignedArgs, updatedAssignedArgs) <- nextArg lnkName allArgs unassignedArgs assignedArgs parentLinkParams
                                          linkArgsPriv lnkName allArgs updatedUnassignedArgs updatedAssignedArgs parentLinkParams
                                        ) <??> "reading lookup arguments"


nextArg :: Text -> [Text] -> [Text] -> ParamMap -> [Text] -> LinksParser ([Text], ParamMap)
nextArg lnkName allArgs argsRemaining assignedArgs parentLinkParams = (
                                                do
                                                  (ky, val) <- singleLineKeyVal parentLinkParams
                                                  if ky `List.elem` argsRemaining then
                                                    do
                                                      let newKeys = List.delete ky argsRemaining
                                                          newMap = Map.insert ky val assignedArgs
                                                      pure (newKeys, newMap)
                                                  else
                                                    fail $ unpack $ "Calling lookup \"" <> lnkName <> "\" with undeclared param name: " <> ky <>
                                                    " - valid keys are: " <> Text.intercalate ", " allArgs
                                                ) <??> (
                                                          let
                                                            toTxt arguments = List.length arguments > 0 ?  Text.intercalate ", " arguments $ "NONE"
                                                            assignedArgsTxt = toTxt $ allArgs List.\\ argsRemaining
                                                            unassignedArgsText = toTxt argsRemaining
                                                          in
                                                            "reading link arguments for " <> lnkName <>
                                                                      "\n\t at failure the following args had been read successfully: " <>
                                                                      assignedArgsTxt <>
                                                                      "\n\t at failure the following args had not been read: " <>
                                                                      unassignedArgsText
                                                        )

singleLineKeyVal :: [Text] -> LinksParser (Text, ElementBlock)
singleLineKeyVal parentLinkParams = (
                                      do
                                        (k, v) <- keyedVal (singleLineFieldVal parentLinkParams False) "single line key: value"
                                        pure (k, v)
                                     ) <??> "Links Parser"

keyedVal :: LinksParser ElementBlock -> Text -> LinksParser (Text, ElementBlock)
keyedVal fieldContentParser errorMessage = (
                                             do
                                               key <- fieldHeader
                                               val <- fieldContentParser
                                               pure (key, val)
                                            ) <??> errorMessage

fieldHeader:: LinksParser Text
fieldHeader = (
               do
                   name <- some $ noneOf  [' ',  '\t', ':', '\n', '\r']
                   _ <- lift colon
                   pure $ pack name
                 ) <??> "field header"

singleLineFieldVal:: [Text] -> Bool  -> LinksParser [BodyElement]
singleLineFieldVal parentLinkParams consumeEol  =
                   (
                     do
                       _ <- many $ lift tabOrSpace
                       linkedWordsSpecial parentLinkParams consumeEol
                     ) <??> "getting singleLineFieldVal "
