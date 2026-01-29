# Getting Started

## Installation

### Cabal

Add to your `.cabal` file:

```cabal
build-depends:
    contra-tracer-contrib
```

### Stack

Add to your `stack.yaml`:

```yaml
extra-deps:
  - contra-tracer-contrib-0.1.0.0
```

And to your `package.yaml`:

```yaml
dependencies:
  - contra-tracer-contrib
```

## Basic Usage

### Import Everything

For convenience, import all modules at once:

```haskell
import Data.Tracer.Contrib
```

This re-exports:

- `Tracer` and `traceWith` from `contra-tracer`
- All utility functions from this library

### Import Specific Modules

For more control over your namespace:

```haskell
import Control.Tracer (Tracer, traceWith)
import Data.Tracer.LogFile (logTracer)
import Data.Tracer.ThreadSafe (newThreadSafeTracer)
import Data.Tracer.Timestamps (addTimestampsTracer)
```

## Common Patterns

### Logging to File

```haskell
import Data.Tracer.Contrib

main :: IO ()
main = logTracer (Just "app.log") $ \tracer -> do
    traceWith tracer "Application started"
    -- ... your app logic
    traceWith tracer "Application finished"
```

### Thread-Safe Logging

```haskell
import Control.Concurrent.Async (forConcurrently_)
import Data.Tracer.Contrib

main :: IO ()
main = logTracer Nothing $ \tracer -> do
    safeTracer <- newThreadSafeTracer tracer
    forConcurrently_ [1..10] $ \i ->
        traceWith safeTracer $ "Processing item " ++ show i
```

### Timestamps

```haskell
import Data.Tracer.Contrib

main :: IO ()
main = logTracer Nothing $ \tracer -> do
    let timestamped = addTimestampsTracer tracer
    traceWith timestamped "Event occurred"
    -- Output: [2025-01-29 10:30:45.123456 UTC] Event occurred
```

### Selective Forwarding

```haskell
import Data.Tracer.Contrib

data Event = Info String | Error String

main :: IO ()
main = logTracer (Just "errors.log") $ \errorTracer ->
    logTracer Nothing $ \allTracer -> do
        let selectErrors = \case
                Error msg -> Just msg
                _ -> Nothing
            tracer = intercept errorTracer selectErrors allTracer
        traceWith tracer (Info "Starting...")
        traceWith tracer (Error "Something went wrong!")
        -- "Starting..." goes to stdout only
        -- "Something went wrong!" goes to both stdout and errors.log
```

## Next Steps

- Browse the [API documentation](api/intercept.md)
- Check the source code for more examples
