{- |
Module      : Data.Tracer.Timestamps
Description : ISO 8601 timestamp prepending
Copyright   : (c) Paolo Veronelli, 2025
License     : Apache-2.0

Provides a tracer transformer that prepends timestamps to log messages.
-}
module Data.Tracer.Timestamps
    ( addTimestampsTracer
    ) where

import Control.Tracer (Tracer, arrow, emit, traceWith)
import Data.Time.Clock (getCurrentTime)

{- | Prepend timestamps to log messages.

The timestamp format is ISO 8601: @[YYYY-MM-DD HH:MM:SS.sss UTC]@

@
timestamped <- addTimestampsTracer logTracer
traceWith timestamped "Hello"
-- Output: [2025-01-15 10:30:45.123456 UTC] Hello
@
-}
addTimestampsTracer
    :: Tracer IO String
    -- ^ underlying string tracer
    -> Tracer IO String
addTimestampsTracer tracer = arrow $ emit $ \msg -> do
    time <- getCurrentTime
    traceWith tracer $ "[" ++ show time ++ "] " ++ msg
