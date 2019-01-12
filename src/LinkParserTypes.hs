

module LinkParserTypes where

import Text.Megaparsec
import Data.Text
import Data.Map.Strict
import Data.Set


data LinkDeclaration = LinkDeclaration {
                                           linkName  :: Text,
                                           params :: [Text],
                                           linkPos :: SourcePos
                                       } deriving (Eq, Ord, Show)

data LinkDefinition = LinkDefinition {
                                       sourceFile :: Text,
                                       declaration :: LinkDeclaration,
                                       body :: ElementBlock
                                     } deriving (Eq, Ord, Show)

data EscapeStatus = Unescaped |
                    SingleWord |
                    MultiWord 
                    deriving (Eq, Ord, Show)

data LinkState = LinkState {
   escapeStatus :: EscapeStatus,
   linkDefs :: LinkDefinitions,
   escapes :: Set Text
}  deriving (Eq, Ord, Show)

data StepType = Instruction |
                Validation
                  deriving (Ord, Eq)

instance Show StepType where
   show Instruction = "#"
   show Validation = "=>"

type LinkDefinitions = Map Text LinkDefinition

type ElementBlock = [BodyElement]

newtype Arg = Arg Text
               deriving (Eq, Ord, Read, Show)

data BodyElement =
                       NewLine |
                       Space |
                       Tab |
                       StepToken StepType|
                       Ref LinkRef |
                       Lit Text |
                       LinkArg Text
                       deriving (Eq, Ord, Show)

data LinkRef = LinkRef  {
                          link :: LinkDefinition,
                          linkArgs :: ParamMap
                        } deriving (Eq, Ord, Show)

type ParamMap = Map Text ElementBlock
