# Data.Tracer.Timestamps

ISO 8601 timestamp prepending.

## Functions

### addTimestampsTracer

```haskell
addTimestampsTracer
    :: Tracer IO String
    -> Tracer IO String
```

Prepend timestamps to log messages.

The timestamp format follows ISO 8601: `[YYYY-MM-DD HH:MM:SS.ssssss UTC]`

**Parameters:**

- `tracer` - underlying string tracer

**Returns:** A tracer that prepends timestamps

## Examples

### Basic Usage

```haskell
import Data.Tracer.Contrib

main :: IO ()
main = logTracer Nothing $ \tracer -> do
    let timestamped = addTimestampsTracer tracer
    traceWith timestamped "Hello, world!"
    -- Output: [2025-01-29 10:30:45.123456 UTC] Hello, world!
```

### Combined with Thread Safety

```haskell
import Control.Concurrent.Async (forConcurrently_)
import Data.Tracer.Contrib

main :: IO ()
main = logTracer (Just "events.log") $ \tracer -> do
    let timestamped = addTimestampsTracer tracer
    safeTracer <- newThreadSafeTracer timestamped

    forConcurrently_ ["alice", "bob", "charlie"] $ \user ->
        traceWith safeTracer $ user ++ " logged in"
```

### Custom Message Types

```haskell
import Data.Tracer.Contrib

data Event = Login String | Logout String | Action String String

showEvent :: Event -> String
showEvent = \case
    Login user -> "LOGIN: " ++ user
    Logout user -> "LOGOUT: " ++ user
    Action user action -> user ++ ": " ++ action

main :: IO ()
main = logTracer Nothing $ \tracer -> do
    let timestamped = addTimestampsTracer tracer
        eventTracer = showEvent >$< timestamped

    traceWith eventTracer (Login "alice")
    traceWith eventTracer (Action "alice" "viewed dashboard")
    traceWith eventTracer (Logout "alice")
```

## Notes

- Uses `getCurrentTime` from `Data.Time.Clock` for UTC timestamps
- The timestamp is captured at trace time, not when the message is eventually written
- Format uses `show` on `UTCTime` - microsecond precision
