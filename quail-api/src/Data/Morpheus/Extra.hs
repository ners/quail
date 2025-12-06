{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Morpheus.Extra where

import Control.Lens.Operators
import Data.Monoid (Any)
import Data.Morpheus.Types
import Data.OpenApi
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Prelude

instance ToSchema GQLRequest where
    declareNamedSchema _ = do
        textSchema <- declareSchemaRef @Text Proxy
        anySchema <- declareSchemaRef @Any Proxy
        pure . NamedSchema (Just "GQLRequest") $
            mempty
                & type_ ?~ OpenApiObject
                & properties
                    .~ [ ("query", textSchema)
                       , ("operationName", textSchema)
                       , ("variables", anySchema)
                       ]
                & required .~ ["query"]

instance ToSchema GQLResponse where
    declareNamedSchema _ = do
        let (NamedSchema _ validValue) =
                NamedSchema (Just "ValidValue") $
                    mempty
                        & type_ ?~ OpenApiObject
                        & properties .~ [("data", Inline (mempty & type_ ?~ OpenApiObject))]
        errorSchema <- declareSchemaRef @GQLError Proxy
        let (NamedSchema _ errors) =
                NamedSchema (Just "Errors") $
                    mempty
                        & type_ ?~ OpenApiArray
                        & items ?~ OpenApiItemsObject errorSchema
        pure . NamedSchema (Just "GQLResponse") $
            mempty
                & type_ ?~ OpenApiObject
                & oneOf ?~ [Inline validValue, Inline errors]

instance ToSchema GQLError where
    declareNamedSchema _ = do
        intSchema <- declareSchemaRef @Int Proxy
        textSchema <- declareSchemaRef @Text Proxy
        let (NamedSchema _ position) =
                NamedSchema (Just "Position") $
                    mempty
                        & type_ ?~ OpenApiObject
                        & properties
                            .~ [ ("line", intSchema)
                               , ("column", intSchema)
                               ]
                        & required .~ ["line", "column"]
        let (NamedSchema _ propName) =
                NamedSchema (Just "PropName") $
                    mempty
                        & type_ ?~ OpenApiObject
                        & oneOf ?~ [intSchema, textSchema]
        pure . NamedSchema (Just "GQLError") $
            mempty
                & type_ ?~ OpenApiObject
                & properties
                    .~ [ ("message", textSchema)
                       ,
                           ( "locations"
                           , Inline $
                                mempty
                                    & type_ ?~ OpenApiArray
                                    & items ?~ OpenApiItemsObject (Inline position)
                           )
                       ,
                           ( "path"
                           , Inline $
                                mempty
                                    & type_ ?~ OpenApiArray
                                    & items ?~ OpenApiItemsObject (Inline propName)
                           )
                       ]
                & required .~ ["message"]
