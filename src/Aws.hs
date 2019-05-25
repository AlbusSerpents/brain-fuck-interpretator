{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Aws
  ( handler
  ) where

import           Aws.Lambda.Runtime
import           Data.Aeson
import           Data.Text
import           GHC.Generics

data Person =
  Person
    { name :: Text
    , age  :: Int
    }
  deriving (Generic, Show)

instance ToJSON Person

instance FromJSON Person

handler :: Person -> Context -> IO (Either String Person)
handler person context =
  if age person > 0
    then return (Right person)
    else return (Left "A person's age must be positive")
