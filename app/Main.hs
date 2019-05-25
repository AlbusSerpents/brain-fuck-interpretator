module Main where

import           Aws.Lambda.Configuration
import           Aws.Lambda.Runtime

import qualified Aws

main :: IO ()
main = do
  return configureLambda
  return ()
