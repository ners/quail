module Effectful.OpenTelemetry.Log
    ( Logging
    , runLogging
    , runMainDefaultLogging
    , emitLogRecord
    , emitLogRecord_
    , SeverityNumber (..)
    , log_
    )
where

import Data.Aeson (ToJSON)
import Data.Aeson.Types (Object, ToJSON (..))
import Data.Functor (void)
import Data.HashMap.Strict qualified as HashMap
import Data.Text qualified as Text
import Effectful
import Effectful.Dispatch.Static
import Effectful.Exception (bracket)
import GHC.Stack (SrcLoc (..), callStack, getCallStack, withFrozenCallStack)
import OpenTelemetry.Internal.Common.Types.Aeson (objectToAnyValueMap, valueToAnyValue)
import OpenTelemetry.Internal.Logs.Types (emptyLogRecordArguments)
import OpenTelemetry.Log (initializeGlobalLoggerProvider)
import OpenTelemetry.Logs.Core
    ( AnyValue (ArrayValue, TextValue)
    , InstrumentationLibrary
    , LogRecordArguments (..)
    , Logger
    , LoggerProvider
    , ReadWriteLogRecord
    , SeverityNumber (..)
    , makeLogger
    , setGlobalLoggerProvider
    , shutdownLoggerProvider
    )
import OpenTelemetry.Logs.Core qualified as OpenTelemetry
import Prelude hiding (log)

data Logging :: Effect

type instance DispatchOf Logging = 'Static 'WithSideEffects

newtype instance StaticRep Logging = Logging Logger

runLogging
    :: (IOE :> es)
    => LoggerProvider
    -> InstrumentationLibrary
    -> Eff (Logging ': es) b
    -> Eff es b
runLogging provider lib =
    evalStaticRep . Logging $ makeLogger provider lib

-- | Bla bla explain this should only be run in main -> the tracer provider cannot be used afterwards
runMainDefaultLogging
    :: (IOE :> es)
    => InstrumentationLibrary
    -> Eff (Logging ': es) b
    -> Eff es b
runMainDefaultLogging lib action =
    bracket (liftIO initializeGlobalLoggerProvider) shutdownLoggerProvider $
        \provider -> do
            setGlobalLoggerProvider provider
            runLogging provider lib action

emitLogRecord :: (Logging :> es) => LogRecordArguments -> Eff es ReadWriteLogRecord
emitLogRecord args = do
    Logging logger <- getStaticRep
    unsafeEff_ $ OpenTelemetry.emitLogRecord logger args

emitLogRecord_ :: (Logging :> es) => LogRecordArguments -> Eff es ()
emitLogRecord_ = void . emitLogRecord

callStackValue :: (HasCallStack) => AnyValue
callStackValue = ArrayValue $ uncurry prettyCallStackLine <$> getCallStack callStack
  where
    prettyCallStackLine fun SrcLoc{..} =
        TextValue . Text.pack . mconcat $
            [ srcLocModule
            , "."
            , fun
            , " at "
            , srcLocFile
            , ":"
            , show srcLocStartLine
            ]

log_ :: (HasCallStack, ToJSON a, Logging :> es) => SeverityNumber -> a -> Object -> Eff es ()
log_ (Just -> severityNumber) (valueToAnyValue . toJSON -> body) (objectToAnyValueMap -> attributes) =
    withFrozenCallStack $
        emitLogRecord_ $
            emptyLogRecordArguments
                { body
                , severityNumber
                , attributes = attributes <> HashMap.singleton "log.call_stack" callStackValue
                }
