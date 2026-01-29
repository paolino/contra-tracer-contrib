# contra-tracer-contrib

[![CI](https://github.com/paolino/contra-tracer-contrib/actions/workflows/CI.yaml/badge.svg)](https://github.com/paolino/contra-tracer-contrib/actions/workflows/CI.yaml)
[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](LICENSE)

Utility modules for [contra-tracer](https://hackage.haskell.org/package/contra-tracer).

## Installation

Add to your `build-depends`:

```cabal
build-depends:
    contra-tracer-contrib
```

## Modules

| Module | Description |
|--------|-------------|
| `Data.Tracer.Intercept` | Selective forwarding via partial mapping |
| `Data.Tracer.LogFile` | File and stdout logging with buffering |
| `Data.Tracer.ThreadSafe` | MVar-based thread-safe wrapper |
| `Data.Tracer.Throttle` | Frequency-based event throttling |
| `Data.Tracer.Timestamp` | Timestamped event wrapper |
| `Data.Tracer.Timestamps` | ISO 8601 timestamp prepending |
| `Data.Tracer.TraceWith` | Pattern synonym for tracer deconstruction |
| `Data.Tracer.Contrib` | Convenience re-exports |

## Quick Example

```haskell
import Data.Tracer.Contrib

main :: IO ()
main = logTracer Nothing $ \tracer -> do
    safeTracer <- newThreadSafeTracer tracer
    let timestamped = addTimestampsTracer safeTracer
    traceWith timestamped "Hello, world!"
    -- Output: [2025-01-29 10:30:45.123456 UTC] Hello, world!
```

## Documentation

Full documentation is available at [paolino.github.io/contra-tracer-contrib](https://paolino.github.io/contra-tracer-contrib/).

## License

Apache-2.0
