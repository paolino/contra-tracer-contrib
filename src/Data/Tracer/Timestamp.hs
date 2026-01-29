{- |
Module      : Data.Tracer.Timestamp
Description : Timestamped event wrapper and tracer
Copyright   : (c) Paolo Veronelli, 2025
License     : Apache-2.0

Provides a wrapper to pair events with their timestamps, and a tracer
transformer that automatically adds timestamps to events.
-}
module Data.Tracer.Timestamp
    ( Timestamped (..)
    , timestampTracer
    ) where

import Control.Tracer (Tracer, traceWith)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Tracer.Internal (mkTracer)

{- | A wrapper that pairs an event with its timestamp.

This is useful for tracers that need to process events based on
when they occurred, such as throttling or time-based filtering.

@
event <- Timestamped \<$\> getCurrentTime \<*\> pure myEvent
traceWith tracer event
@
-}
data Timestamped a = Timestamped
    { timestampedTime :: UTCTime
    -- ^ when the event occurred
    , timestampedEvent :: a
    -- ^ the event payload
    }
    deriving (Show, Eq)

{- | Wrap events with their current timestamp.

Takes a downstream tracer that expects timestamped events and returns
a tracer that accepts raw events, automatically adding timestamps.

@
downstream <- throttleByFrequency [matcher] baseTracer
let tracer = timestampTracer downstream
traceWith tracer "Hello"  -- automatically timestamped
@

This is commonly composed with 'throttleByFrequency' which expects
'Timestamped a' inputs for time-based throttling.
-}
timestampTracer :: Tracer IO (Timestamped a) -> Tracer IO a
timestampTracer downstream = mkTracer $ \event -> do
    time <- getCurrentTime
    traceWith downstream $ Timestamped time event
