module Data.Tracer.ThreadSafe
    ( newThreadSafeTracer
    ) where

-- \|
-- Module      : Data.Tracer.ThreadSafe
-- Description : MVar-based thread-safe wrapper
-- Copyright   : (c) Paolo Veronelli, 2025
-- License     : Apache-2.0
--
-- Provides a thread-safe wrapper for tracers using MVar synchronization.

import Control.Concurrent.MVar (newMVar, withMVar)
import Control.Tracer (Tracer, arrow, emit, traceWith)

{- | Create a thread-safe tracer by synchronizing access to the underlying
tracer.

This ensures that trace calls from multiple threads are serialized,
preventing interleaved output when the underlying tracer is not thread-safe.

@
safeTracer <- newThreadSafeTracer unsafeTracer
forConcurrently_ items $ \\item ->
    traceWith safeTracer (show item)
@
-}
newThreadSafeTracer
    :: Tracer IO a
    -- ^ the underlying (potentially unsafe) tracer
    -> IO (Tracer IO a)
newThreadSafeTracer unsafe = do
    mvar <- newMVar ()
    return $ arrow $ emit $ \a ->
        withMVar mvar $ \_ ->
            traceWith unsafe a
