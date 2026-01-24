{-# OPTIONS_GHC -Wno-orphans #-}

module OpenTelemetry.Internal.Common.Types.Aeson where

import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Scientific (floatingOrInteger)
import Data.Text (Text)
import Data.Vector qualified as Vector
import OpenTelemetry.Internal.Common.Types
import Prelude

objectToAnyValueMap :: Object -> HashMap Text AnyValue
objectToAnyValueMap o =
    HashMap.fromList $
        [ (Key.toText key, valueToAnyValue val)
        | (key, val) <- KeyMap.toList o
        ]

valueToAnyValue :: Value -> AnyValue
valueToAnyValue (Object o) = HashMapValue $ objectToAnyValueMap o
valueToAnyValue (Array v) = ArrayValue $ valueToAnyValue <$> Vector.toList v
valueToAnyValue (String s) = TextValue s
valueToAnyValue (Number n) = either DoubleValue IntValue . floatingOrInteger $ n
valueToAnyValue (Bool b) = BoolValue b
valueToAnyValue Null = NullValue

instance FromJSON AnyValue where
    parseJSON = pure . valueToAnyValue
