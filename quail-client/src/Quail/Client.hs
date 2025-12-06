{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-deriving-strategies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Quail.Client where

import Data.Morpheus.Client (declareLocalTypesInline, raw)
import Data.Text (Text)
import Quail.GqlSchema qualified as Quail
import Prelude

declareLocalTypesInline
    GQL_SCHEMA_FILE
    [raw|
    query AllUrls
      {
        getAllUrls {
          stub
          target
        }
      }
  |]
