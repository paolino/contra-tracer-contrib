# Data.Tracer.LogFile

File and stdout logging with buffering.

## Functions

### logFileTracer

```haskell
logFileTracer
    :: FilePath
    -> (Tracer IO String -> IO a)
    -> IO a
```

Create a tracer that logs to a file with line buffering.

The file is opened in append mode and closed when the continuation returns.

**Parameters:**

- `filepath` - path to log file
- `continuation` - receives the tracer

**Returns:** Result of the continuation

### logTracer

```haskell
logTracer
    :: Maybe FilePath
    -> (Tracer IO String -> IO a)
    -> IO a
```

Create a tracer that logs to a file if a filepath is provided, otherwise logs to stdout with line buffering.

**Parameters:**

- `filepath` - optional path to log file (`Nothing` for stdout)
- `continuation` - receives the tracer

**Returns:** Result of the continuation

## Examples

### Basic File Logging

```haskell
import Data.Tracer.Contrib

main :: IO ()
main = logFileTracer "app.log" $ \tracer -> do
    traceWith tracer "Application started"
    -- ... application logic
    traceWith tracer "Application finished"
```

### Configurable Output

```haskell
import Data.Tracer.Contrib
import System.Environment (lookupEnv)

main :: IO ()
main = do
    logFile <- lookupEnv "LOG_FILE"
    logTracer logFile $ \tracer -> do
        traceWith tracer "Starting..."
        -- LOG_FILE not set -> stdout
        -- LOG_FILE=app.log -> writes to app.log
```

### Multiple Log Files

```haskell
import Data.Tracer.Contrib

main :: IO ()
main =
    logFileTracer "errors.log" $ \errorTracer ->
    logFileTracer "debug.log" $ \debugTracer ->
    logTracer Nothing $ \consoleTracer -> do
        -- Use different tracers for different purposes
        traceWith consoleTracer "User-visible message"
        traceWith debugTracer "Debug info"
        traceWith errorTracer "Error occurred"
```

## Notes

- Files are opened in **append mode** - existing content is preserved
- **Line buffering** is used - each message is flushed after the newline
- Files are automatically closed when the continuation returns
- Parent directories must exist (no automatic creation)
