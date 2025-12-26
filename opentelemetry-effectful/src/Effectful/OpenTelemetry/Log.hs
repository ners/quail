{-# OPTIONS_GHC -Wno-orphans #-}

module Effectful.OpenTelemetry.Log
    ( Log
    , runLog
    , Severity (..)
    , logTrace
    , logDebug
    , logInfo
    , logWarn
    , logError
    , logFatal
    )
where

import Colog.Core qualified as Colog
import Data.Aeson.Encode.Pretty qualified as Aeson
import Data.Aeson.Types
    ( FromJSON (..)
    , KeyValue (..)
    , KeyValueOmit (..)
    , Object
    , ToJSON (..)
    , Value
    )
import Data.Aeson.Types qualified as Aeson
import Data.ByteString.Lazy.Char8 qualified as LazyByteString
import Data.Coerce (coerce)
import Data.Text (Text)
import Effectful (Eff, IOE, liftIO, (:>))
import Effectful.Colog qualified as Colog
import Effectful.Dispatch.Static (unsafeEff_)
import GHC.Stack
    ( CallStack
    , HasCallStack
    , SrcLoc
    , callStack
    , fromCallSiteList
    , getCallStack
    , withFrozenCallStack
    )
import OpenTelemetry.Common (Timestamp (..), TraceFlags)
import OpenTelemetry.Context qualified as OpenTelemetry
import OpenTelemetry.Context.ThreadLocal qualified as OpenTelemetry
import OpenTelemetry.Trace.Core (getTimestamp, traceFlagsValue)
import OpenTelemetry.Trace.Core qualified as OpenTelemetry
import OpenTelemetry.Trace.Id
    ( Base (Base16)
    , SpanId
    , TraceId
    , spanIdBaseEncodedText
    , traceIdBaseEncodedText
    )
import System.Clock (toNanoSecs)
import Prelude

data Severity
    = TRACE
    | TRACE2
    | TRACE3
    | TRACE4
    | DEBUG
    | DEBUG2
    | DEBUG3
    | DEBUG4
    | INFO
    | INFO2
    | INFO3
    | INFO4
    | WARN
    | WARN2
    | WARN3
    | WARN4
    | ERROR
    | ERROR2
    | ERROR3
    | ERROR4
    | FATAL
    | FATAL2
    | FATAL3
    | FATAL4
    deriving stock (Eq, Ord, Bounded, Enum, Show)

severityNumber :: Severity -> Int
severityNumber = succ . fromEnum

data Message = Message
    { timestamp :: Maybe Timestamp
    , severity :: Maybe Severity
    , traceId :: Maybe TraceId
    , spanId :: Maybe SpanId
    , traceFlags :: Maybe TraceFlags
    , name :: Maybe Text
    , body :: Maybe Value
    , resource :: Maybe Object
    , attributes :: Maybe Object
    }

instance ToJSON Message where
    toJSON Message{..} =
        Aeson.Object . mconcat $
            [ "Timestamp" .?= (toNanoSecs . coerce <$> timestamp)
            , "TraceId" .?= (traceIdBaseEncodedText Base16 <$> traceId)
            , "SpanId" .?= (spanIdBaseEncodedText Base16 <$> spanId)
            , "TraceFlags" .?= (traceFlagsValue <$> traceFlags)
            , "SeverityText" .?= (show <$> severity)
            , "SeverityNumber" .?= (severityNumber <$> severity)
            , "Name" .?= name
            , "Body" .?= body
            , "Resource" .?= resource
            , "Attributes" .?= attributes
            ]

type Log = Colog.Log (CallStack, Message)

emptyMessage :: Message
emptyMessage =
    Message
        { timestamp = Nothing
        , severity = Nothing
        , traceId = Nothing
        , spanId = Nothing
        , traceFlags = Nothing
        , name = Nothing
        , body = Nothing
        , resource = Nothing
        , attributes = Nothing
        }

instance FromJSON SrcLoc

instance ToJSON SrcLoc

instance FromJSON CallStack where
    parseJSON = fmap fromCallSiteList . parseJSON

instance ToJSON CallStack where
    toJSON = toJSON . getCallStack

newMessage :: (ToJSON body) => Severity -> Text -> body -> Eff es Message
newMessage (Just -> severity) (Just -> name) (toJSON -> Just -> body) = do
    timestamp <- Just <$> unsafeEff_ getTimestamp
    context <- unsafeEff_ OpenTelemetry.getContext
    spanContext <- unsafeEff_ . mapM OpenTelemetry.getSpanContext . OpenTelemetry.lookupSpan $ context
    pure
        emptyMessage
            { timestamp
            , severity
            , traceId = OpenTelemetry.traceId <$> spanContext
            , spanId = OpenTelemetry.spanId <$> spanContext
            , traceFlags = OpenTelemetry.traceFlags <$> spanContext
            , name
            , body
            }

logMaybeMessage :: (IOE :> es) => Colog.LogAction (Eff es) (Maybe Message)
logMaybeMessage = Colog.LogAction \case
    Nothing -> pure ()
    Just msg -> liftIO . LazyByteString.putStrLn . Aeson.encodePretty $ msg

severityAtLeast :: Severity -> Message -> Bool
severityAtLeast s = maybe False (s <=) . severity

filterSeverity :: Severity -> Message -> Maybe Message
filterSeverity verbosity m | severityAtLeast verbosity m = Just m
filterSeverity _ _ = Nothing

addCallStack :: Severity -> (CallStack, Message) -> Message
addCallStack stackVerbosity (callStack, message)
    | severityAtLeast stackVerbosity message =
        message{attributes = withCallStack $ attributes message}
  where
    callStackObj = "call_stack" .= callStack
    withCallStack Nothing = Just callStackObj
    withCallStack (Just attributes) = Just $ attributes <> callStackObj
addCallStack _ (_, message) = message

runLog :: (IOE :> es) => Severity -> Severity -> Eff (Log ': es) a -> Eff es a
runLog verbosity stackVerbosity =
    Colog.runLogAction $
        Colog.cmap (filterSeverity verbosity . addCallStack stackVerbosity) logMaybeMessage

logMsg :: (HasCallStack, Log :> es) => Message -> Eff es ()
logMsg = Colog.logMsg . (callStack,)

logTrace :: (HasCallStack, ToJSON body, Log :> es) => Text -> body -> Eff es ()
logTrace name body = withFrozenCallStack $ logMsg =<< newMessage TRACE name body

logDebug :: (HasCallStack, ToJSON body, Log :> es) => Text -> body -> Eff es ()
logDebug name body = withFrozenCallStack $ logMsg =<< newMessage DEBUG name body

logInfo :: (HasCallStack, ToJSON body, Log :> es) => Text -> body -> Eff es ()
logInfo name body = withFrozenCallStack $ logMsg =<< newMessage INFO name body

logWarn :: (HasCallStack, ToJSON body, Log :> es) => Text -> body -> Eff es ()
logWarn name body = withFrozenCallStack $ logMsg =<< newMessage WARN name body

logError :: (HasCallStack, ToJSON body, Log :> es) => Text -> body -> Eff es ()
logError name body = withFrozenCallStack $ logMsg =<< newMessage ERROR name body

logFatal :: (HasCallStack, ToJSON body, Log :> es) => Text -> body -> Eff es ()
logFatal name body = withFrozenCallStack $ logMsg =<< newMessage FATAL name body
