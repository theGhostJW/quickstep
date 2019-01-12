
{-# LANGUAGE OverloadedStrings #-}


module PlanExpander where

import           Data.List
import           Data.Map.Strict
import           Data.Monoid
import           Data.Text
import           Foundation.Extended (Truthy (..))
import           LamdahofUtils       as Utils
import           LinkParserTypes

expandBody :: ElementBlock -> Text
expandBody = expandBodyWithState baseExpandState

expandBodyWithState :: ExpandState -> ElementBlock -> Text
expandBodyWithState xState elements = text $ Data.List.foldl' expandStep xState elements

baseExpandState :: ExpandState
baseExpandState = ExpandState {
                                 inStepToken = False,
                                 indent = 0,
                                 parentIndent = 0,
                                 atLineStart = True,
                                 text = "",
                                 inScopeArgs = Data.Map.Strict.empty
                              }

data ExpandState = ExpandState {
   text         :: Text,
   inStepToken  :: Bool,
   indent       :: Int,
   parentIndent :: Int,
   atLineStart  :: Bool,
   inScopeArgs  :: ParamMap
} deriving Show

clearText :: ExpandState -> ExpandState
clearText s = s {
               text = ""
         }

indentTxt :: ExpandState -> Text
indentTxt es =
   atLineStart es ?
               Data.Text.replicate (totalIndent es) "\t" $
               ""

totalIndent :: ExpandState -> Int
totalIndent es = indent es + parentIndent es

expandStepType :: StepType -> Text
expandStepType Validation  = "=>"
expandStepType Instruction = "#"

expandStep :: ExpandState -> BodyElement -> ExpandState
expandStep endState elm =
   let
      txtSoFar = text endState
   in
      case elm of
         NewLine -> endState {
            atLineStart = True,
            inStepToken = False,
            indent = 0,
            text = txtSoFar <> "\n"
         }
         Space -> endState {text = inStepToken endState ? txtSoFar $ txtSoFar <> " "}
         Tab -> endState {
            indent = atLineStart endState ? succ (indent endState) $ indent endState,
            text = atLineStart endState ? txtSoFar $ txtSoFar <> "\t"
         }
         StepToken st ->  endState {
            atLineStart = False,
            inStepToken = True,
            text = txtSoFar <> indentTxt endState <> (inStepToken endState ? "" $ expandStepType st)
         }
         Ref lr -> endState {
            text = txtSoFar <> expandLinkRef lr endState
         }
         LinkArg argTxt -> endState {
            text = txtSoFar <> expandArg argTxt endState
         }
         Lit txt -> endState {
            atLineStart = False,
            inStepToken = False,
            text = txtSoFar <> indentTxt endState <> (inStepToken endState ? (" " <> capsFirst txt) $ txt)
         }

substituteSingleElement :: ParamMap -> BodyElement -> ElementBlock
substituteSingleElement callerArgs (LinkArg argTxt) = findWithDefault [Lit $ "Software Bug Key: \"" <> argTxt <> "\" is not in available args: " <> pack (show callerArgs)] argTxt callerArgs
substituteSingleElement _ element = [element]

substituteArgs :: ParamMap -> ElementBlock -> ElementBlock
substituteArgs callerArgs elementBlock = substituteSingleElement callerArgs =<< elementBlock

expandLinkRef :: LinkRef -> ExpandState -> Text
expandLinkRef linkRef exState = let
                                    myArgs = linkArgs linkRef
                                    argReplacer = substituteArgs $ inScopeArgs exState
                                    myReplacedArgs = Data.Map.Strict.map argReplacer myArgs
                                    passThroughState = clearText $ exState {
                                                                              indent = 0,
                                                                              parentIndent = totalIndent exState,
                                                                              inScopeArgs = myReplacedArgs
                                                                           }
                                 in
                                    expandBodyWithState passThroughState $ body $ link linkRef

expandArg :: Text -> ExpandState -> Text
expandArg key exState = let
                           args = inScopeArgs exState
                           linkVal = findWithDefault [Lit $ "Software Bug Key: \"" <> key <> "\" is not in available args: " <> pack (show args)] key args
                           newState = clearText exState
                         in
                           expandBodyWithState newState linkVal

capsFirst :: Text -> Text
capsFirst txt = Data.Text.null txt ? txt $ (toUpper . Data.Text.singleton $ Data.Text.head txt) <> Data.Text.tail txt
