# Data.Tracer.Throttle

Frequency-based event throttling.

## Types

### Throttled

```haskell
data Throttled a = Throttled
    { throttledEvent :: Timestamp a
    , throttledDropped :: Int
    }
```

Result of throttling an event. When an event passes through the throttle,
it includes a count of how many events were dropped since the last emission.

## Functions

### throttleByFrequency

```haskell
throttleByFrequency
    :: [a -> Maybe Double]
    -> Tracer IO (Throttled a)
    -> IO (Tracer IO (Timestamp a))
```

Create a tracer that throttles events based on frequency limits.

Each matcher function defines a throttle category. When a matcher returns
`Just frequency`, events matching that category are limited to that frequency
(in Hz, i.e., events per second).

**Behavior:**

- Events that don't match any category pass through immediately
- The first event in each category always passes through
- Subsequent events within the throttle interval are dropped
- When an event finally passes, it reports how many were dropped

**Parameters:**

- `matchers` - functions returning frequency in Hz (events/second)
- `downstream` - tracer receiving throttled events

**Returns:** A tracer that throttles timestamped events

## Examples

### Basic Throttling

```haskell
import Data.Tracer.Contrib
import Data.Time.Clock (getCurrentTime)

-- Throttle all events to max 1 per second
main :: IO ()
main = do
    let matcher _ = Just 1.0  -- 1 Hz
    tracer <- throttleByFrequency [matcher] downstream

    now <- getCurrentTime
    traceWith tracer (Timestamp now "event1")  -- passes through
    traceWith tracer (Timestamp now "event2")  -- dropped
    -- After 1 second...
    later <- getCurrentTime
    traceWith tracer (Timestamp later "event3")  -- passes with dropped=1
```

### Category-Based Throttling

```haskell
import Data.List (isInfixOf)
import Data.Tracer.Contrib

data LogLevel = Debug | Info | Warning | Error
    deriving (Eq)

data LogEntry = LogEntry LogLevel String

-- Throttle errors to 1/sec, warnings to 2/sec
matchError :: LogEntry -> Maybe Double
matchError (LogEntry Error _) = Just 1.0
matchError _ = Nothing

matchWarning :: LogEntry -> Maybe Double
matchWarning (LogEntry Warning _) = Just 2.0
matchWarning _ = Nothing

main :: IO ()
main = do
    tracer <- throttleByFrequency [matchError, matchWarning] downstream
    -- Errors throttled independently from warnings
    -- Debug and Info pass through unthrottled
```

### Progress Reporting

```haskell
import Data.Tracer.Contrib

-- Throttle progress updates to avoid flooding logs
main :: IO ()
main = do
    let matchProgress msg
            | "progress" `isInfixOf` msg = Just 0.5  -- max 1 every 2 seconds
            | otherwise = Nothing

    tracer <- throttleByFrequency [matchProgress] logTracer

    forM_ [1..1000] $ \i -> do
        now <- getCurrentTime
        traceWith tracer $ Timestamp now $ "progress: " ++ show i ++ "/1000"
```

## Notes

- Uses event timestamps (not system time) for deterministic, testable behavior
- Each matcher defines an independent throttle category
- Multiple matchers can match the same event - first match wins
- Non-matching events always pass through immediately with `throttledDropped = 0`
