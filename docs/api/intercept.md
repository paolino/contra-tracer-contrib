# Data.Tracer.Intercept

Selective forwarding via partial mapping.

## Functions

### intercept

```haskell
intercept
    :: Monad m
    => Tracer m b
    -> (a -> Maybe b)
    -> Tracer m a
    -> Tracer m a
```

Modify a `Tracer` to emit to another `Tracer` based on a partial mapping function.

The intercepted tracer continues to emit all events to the primary tracer, but also forwards events to a secondary tracer when the mapping function returns `Just`.

**Parameters:**

- `secondary` - receives transformed events when mapping succeeds
- `mapping` - partial function to transform events
- `primary` - receives all events unchanged

**Returns:** A new tracer that forwards to both tracers

## Examples

### Logging Errors to a Separate File

```haskell
import Data.Tracer.Contrib

data LogLevel = Debug | Info | Warning | Error
    deriving (Eq, Show)

data LogEntry = LogEntry LogLevel String

main :: IO ()
main = logTracer (Just "errors.log") $ \errorTracer ->
    logTracer (Just "all.log") $ \allTracer -> do
        let onlyErrors (LogEntry lvl msg)
                | lvl == Error = Just msg
                | otherwise = Nothing
            logger = intercept errorTracer onlyErrors $
                fmap show allTracer

        traceWith logger (LogEntry Info "Starting")
        traceWith logger (LogEntry Error "Connection failed")
        -- "Starting" -> all.log only
        -- "Connection failed" -> both files
```

### Metrics Collection

```haskell
import Data.Tracer.Contrib

data Event
    = UserLogin String
    | PageView String
    | Purchase Double
    | Error String

main :: IO ()
main = do
    metricsRef <- newIORef (0 :: Int)
    let metricsTracer = Tracer $ \_ ->
            modifyIORef' metricsRef (+ 1)
        isPurchase (Purchase _) = Just ()
        isPurchase _ = Nothing

    logTracer Nothing $ \logTracer' -> do
        let tracer = intercept metricsTracer isPurchase $
                fmap show logTracer'

        traceWith tracer (UserLogin "alice")
        traceWith tracer (Purchase 99.99)
        traceWith tracer (Purchase 49.99)

        count <- readIORef metricsRef
        print count  -- 2
```
