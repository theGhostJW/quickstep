{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE LambdaCase #-}

module ResultParserShared where

import           Data.Text hiding (count)

data RawTestResult = TestResult {
  title   :: Text,
  content :: Text
}  deriving (Eq, Ord, Show)

type RawTestResults = [RawTestResult]
