module Main where

import Data.Morpheus.Client (forEach, request)
import Quail.Client
import Prelude

main :: IO ()
main = forEach print =<< request @AllUrls "http://localhost:8081/gql" ()
