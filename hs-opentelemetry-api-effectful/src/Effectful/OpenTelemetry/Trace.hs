{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-type-patterns #-}

module Effectful.OpenTelemetry.Trace
    ( Tracing
    , runTracing
    , SpanArguments (..)
    , SpanKind (..)
    , defaultSpanArguments
    , runDefaultTracing
    , runMainDefaultTracing
    , initializeGlobalTracerProvider
    , getGlobalTracerProvider
    , shutdownTracerProvider
    , inSpan
    , inSpan'
    , inSpan''
    )
where

import Data.Text (Text)
import Effectful
import Effectful.Dispatch.Static
import Effectful.Exception (bracket)
import OpenTelemetry.Trace
    ( InstrumentationLibrary
    , Span
    , SpanArguments (..)
    , SpanKind (..)
    , Tracer
    , TracerOptions
    , TracerProvider
    , defaultSpanArguments
    , getGlobalTracerProvider
    , initializeGlobalTracerProvider
    , makeTracer
    , shutdownTracerProvider
    , tracerOptions
    )
import OpenTelemetry.Trace.Core qualified as OpenTelemetry
import Prelude

data Tracing :: Effect

type instance DispatchOf Tracing = 'Static 'WithSideEffects

newtype instance StaticRep Tracing = Tracing Tracer

runTracing
    :: (IOE :> es)
    => TracerProvider
    -> InstrumentationLibrary
    -> TracerOptions
    -> Eff (Tracing ': es) b
    -> Eff es b
runTracing provider lib opts =
    evalStaticRep . Tracing $ makeTracer provider lib opts

runDefaultTracing
    :: (IOE :> es)
    => TracerProvider
    -> InstrumentationLibrary
    -> Eff (Tracing ': es) b
    -> Eff es b
runDefaultTracing provider lib = runTracing provider lib tracerOptions

-- | Bla bla explain this should only be run in main -> the tracer provider cannot be used afterwards
runMainDefaultTracing
    :: (IOE :> es)
    => InstrumentationLibrary
    -> Eff (Tracing ': es) b
    -> Eff es b
runMainDefaultTracing lib action =
    bracket (liftIO initializeGlobalTracerProvider) shutdownTracerProvider $
        \provider -> runDefaultTracing provider lib action

perform :: (Tracing :> es) => (Tracer -> (forall r. Eff es r -> IO r) -> IO a) -> Eff es a
perform f = do
    Tracing tracer <- getStaticRep
    unsafeConcUnliftIO Ephemeral (Limited 1) (f tracer)

inSpan :: (Tracing :> es) => Text -> SpanArguments -> Eff es a -> Eff es a
inSpan spanName args a = perform \tracer unlift -> OpenTelemetry.inSpan tracer spanName args (unlift a)

inSpan' :: (Tracing :> es) => Text -> SpanArguments -> (Span -> Eff es a) -> Eff es a
inSpan' spanName args f = perform \tracer unlift -> OpenTelemetry.inSpan' tracer spanName args (unlift . f)

inSpan'' :: (Tracing :> es) => Text -> SpanArguments -> (Span -> Eff es a) -> Eff es a
inSpan'' spanName args f = perform \tracer unlift -> OpenTelemetry.inSpan'' tracer spanName args (unlift . f)
