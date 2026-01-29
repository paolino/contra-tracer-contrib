# Data.Tracer.ThreadSafe

MVar-based thread-safe wrapper.

## Functions

### newThreadSafeTracer

```haskell
newThreadSafeTracer
    :: Tracer IO a
    -> IO (Tracer IO a)
```

Create a thread-safe tracer by synchronizing access to the underlying tracer.

This ensures that trace calls from multiple threads are serialized, preventing interleaved output when the underlying tracer is not thread-safe.

**Parameters:**

- `unsafe` - the underlying (potentially unsafe) tracer

**Returns:** A new thread-safe tracer

## Examples

### Concurrent Logging

```haskell
import Control.Concurrent.Async (forConcurrently_)
import Data.Tracer.Contrib

main :: IO ()
main = logTracer Nothing $ \tracer -> do
    safeTracer <- newThreadSafeTracer tracer
    forConcurrently_ [1..10] $ \i ->
        traceWith safeTracer $ "Thread " ++ show i ++ " reporting"
```

### With Timestamps

```haskell
import Control.Concurrent.Async (forConcurrently_)
import Data.Tracer.Contrib

main :: IO ()
main = logTracer Nothing $ \tracer -> do
    -- Apply transformations before making thread-safe
    let timestamped = addTimestampsTracer tracer
    safeTracer <- newThreadSafeTracer timestamped

    forConcurrently_ [1..5] $ \i -> do
        traceWith safeTracer $ "Start " ++ show i
        threadDelay 1000
        traceWith safeTracer $ "End " ++ show i
```

## Notes

- Each call to `newThreadSafeTracer` creates a new independent MVar
- The synchronization serializes trace calls - high-frequency tracing from many threads may become a bottleneck
- Only the `traceWith` call is synchronized - any work done to produce the message happens before acquiring the lock
- For highest performance with many threads, consider batching or async logging
