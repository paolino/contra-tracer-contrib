# contra-tracer-contrib

Utility modules for [contra-tracer](https://hackage.haskell.org/package/contra-tracer).

## Overview

This library provides commonly needed functionality when working with `contra-tracer`:

| Module | Description |
|--------|-------------|
| [`Data.Tracer.Intercept`](api/intercept.md) | Selective forwarding via partial mapping |
| [`Data.Tracer.LogFile`](api/logfile.md) | File and stdout logging with buffering |
| [`Data.Tracer.ThreadSafe`](api/thread-safe.md) | MVar-based thread-safe wrapper |
| [`Data.Tracer.Throttle`](api/throttle.md) | Frequency-based event throttling |
| [`Data.Tracer.Timestamp`](api/timestamp.md) | Timestamped event wrapper |
| [`Data.Tracer.Timestamps`](api/timestamps.md) | ISO 8601 timestamp prepending |
| [`Data.Tracer.TraceWith`](api/trace-with.md) | Pattern synonym for tracer deconstruction |

## Quick Start

```haskell
import Data.Tracer.Contrib

main :: IO ()
main = logTracer Nothing $ \tracer -> do
    safeTracer <- newThreadSafeTracer tracer
    let timestamped = addTimestampsTracer safeTracer
    traceWith timestamped "Hello, world!"
```

Output:
```
[2025-01-29 10:30:45.123456 UTC] Hello, world!
```

## Installation

See [Getting Started](getting-started.md) for installation instructions.

## License

Apache-2.0
