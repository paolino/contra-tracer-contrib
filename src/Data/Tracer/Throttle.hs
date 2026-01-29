{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}

{- |
Module      : Data.Tracer.Throttle
Description : Frequency-based event throttling
Copyright   : (c) Paolo Veronelli, 2025
License     : Apache-2.0

Provides a tracer that throttles events based on frequency limits.
Uses event timestamps for deterministic, testable behavior.
-}
module Data.Tracer.Throttle
    ( Throttled (..)
    , throttleByFrequency
    ) where

import Control.Tracer (Tracer, arrow, emit, traceWith)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Time.Clock (UTCTime, diffUTCTime)
import Data.Tracer.Timestamp (Timestamp (..))

{- | Result of throttling an event.

When an event passes through the throttle, it includes a count of
how many events were dropped since the last emission.
-}
data Throttled a = Throttled
    { throttledEvent :: Timestamp a
    -- ^ the event that passed through
    , throttledDropped :: Int
    -- ^ number of events dropped since last emission
    }
    deriving (Show, Eq)

-- | State for a single throttle category.
data ThrottleState = ThrottleState
    { lastEmitTime :: UTCTime
    -- ^ when we last emitted for this category
    , droppedCount :: Int
    -- ^ events dropped since last emission
    }

{- | Create a tracer that throttles events based on frequency limits.

Each matcher function defines a throttle category. When a matcher
returns @Just frequency@, events matching that category are limited
to that frequency (in Hz, i.e., events per second).

Events that don't match any category pass through immediately.
The first event in each category always passes through.
Subsequent events within the throttle interval are dropped.
When an event finally passes, it reports how many were dropped.

@
let matchErrors msg
        | "error" \`isInfixOf\` msg = Just 1.0  -- max 1 per second
        | otherwise = Nothing
tracer <- throttleByFrequency [matchErrors] downstream
@
-}
throttleByFrequency
    :: [a -> Maybe Double]
    -- ^ matchers returning frequency in Hz (events/second)
    -> Tracer IO (Throttled a)
    -- ^ downstream tracer
    -> IO (Tracer IO (Timestamp a))
throttleByFrequency matchers downstream = do
    stateRef <- newIORef (Map.empty :: Map Int ThrottleState)
    return $ arrow $ emit $ \event -> do
        let ts = timestampTime event
        case findMatch matchers (timestampEvent event) of
            Nothing ->
                -- No matcher - pass through immediately
                traceWith downstream $
                    Throttled{throttledEvent = event, throttledDropped = 0}
            Just (categoryIdx, frequency) -> do
                let interval = 1.0 / frequency
                state <- readIORef stateRef
                case Map.lookup categoryIdx state of
                    Nothing -> do
                        -- First event in category - emit and record
                        modifyIORef' stateRef $
                            Map.insert
                                categoryIdx
                                ThrottleState
                                    { lastEmitTime = ts
                                    , droppedCount = 0
                                    }
                        traceWith downstream $
                            Throttled
                                { throttledEvent = event
                                , throttledDropped = 0
                                }
                    Just ThrottleState{lastEmitTime, droppedCount} -> do
                        let elapsed =
                                realToFrac (diffUTCTime ts lastEmitTime)
                                    :: Double
                        if elapsed >= interval
                            then do
                                -- Interval passed - emit with drop count
                                modifyIORef' stateRef $
                                    Map.insert
                                        categoryIdx
                                        ThrottleState
                                            { lastEmitTime = ts
                                            , droppedCount = 0
                                            }
                                traceWith downstream $
                                    Throttled
                                        { throttledEvent = event
                                        , throttledDropped = droppedCount
                                        }
                            else do
                                -- Within interval - drop and count
                                modifyIORef' stateRef $
                                    Map.adjust
                                        ( \s ->
                                            s{droppedCount = droppedCount + 1}
                                        )
                                        categoryIdx

-- | Find the first matching category and its frequency.
findMatch :: [a -> Maybe Double] -> a -> Maybe (Int, Double)
findMatch matchers event = go 0 matchers
  where
    go _ [] = Nothing
    go idx (m : ms) = case m event of
        Just freq -> Just (idx, freq)
        Nothing -> go (idx + 1) ms
