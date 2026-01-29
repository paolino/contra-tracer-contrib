# Data.Tracer.Timestamp

Timestamped event wrapper.

## Types

### Timestamp

```haskell
data Timestamp a = Timestamp
    { timestampTime :: UTCTime
    , timestampEvent :: a
    }
```

A wrapper that pairs an event with its timestamp.

This is useful for tracers that need to process events based on when they
occurred, such as throttling or time-based filtering.

**Fields:**

- `timestampTime` - when the event occurred
- `timestampEvent` - the event payload

## Examples

### Creating Timestamped Events

```haskell
import Data.Tracer.Contrib
import Data.Time.Clock (getCurrentTime)

main :: IO ()
main = do
    now <- getCurrentTime
    let event = Timestamp
            { timestampTime = now
            , timestampEvent = "Something happened"
            }
    traceWith tracer event
```

### With Applicative Style

```haskell
import Data.Tracer.Contrib
import Data.Time.Clock (getCurrentTime)

mkTimestamp :: IO a -> IO (Timestamp a)
mkTimestamp ma = Timestamp <$> getCurrentTime <*> ma

main :: IO ()
main = do
    event <- mkTimestamp (pure "my event")
    traceWith tracer event
```

### Pattern Matching

```haskell
processEvent :: Timestamp String -> IO ()
processEvent Timestamp{timestampTime, timestampEvent} = do
    putStrLn $ "At " ++ show timestampTime ++ ": " ++ timestampEvent
```

## Notes

- Derives `Show` and `Eq` for easy testing and debugging
- Commonly used with `Data.Tracer.Throttle` for frequency-based throttling
- The timestamp should reflect when the event occurred, not when it was traced
