{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-deriving-strategies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Quail.GqlSchema where

import Data.Bool (Bool)
import Data.Morpheus.Document (importGQLDocument)
import Data.Text (Text)

importGQLDocument GQL_SCHEMA_FILE
