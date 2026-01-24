{-# LANGUAGE CPP #-}
{-# LANGUAGE DuplicateRecordFields #-}

module GraphQL where

import Miso.GraphQL.TH (documentFile)
import Miso.Prelude

documentFile GQL_SCHEMA_FILE
