module Data.Tracer.Timestamp
    ( Timestamp (..)
    ) where

-- \|
-- Module      : Data.Tracer.Timestamp
-- Description : Timestamped event wrapper
-- Copyright   : (c) Paolo Veronelli, 2025
-- License     : Apache-2.0
--
-- Provides a simple wrapper to pair events with their timestamps.

import Data.Time.Clock (UTCTime)

{- | A wrapper that pairs an event with its timestamp.

This is useful for tracers that need to process events based on
when they occurred, such as throttling or time-based filtering.

@
event <- mkTimestamp \<$\> getCurrentTime \<*\> pure myEvent
traceWith tracer event
@
-}
data Timestamp a = Timestamp
    { timestampTime :: UTCTime
    -- ^ when the event occurred
    , timestampEvent :: a
    -- ^ the event payload
    }
    deriving (Show, Eq)
