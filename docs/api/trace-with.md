# Data.Tracer.TraceWith

Pattern synonym for tracer deconstruction.

## Pattern Synonym

### TraceWith

```haskell
pattern TraceWith
    :: Tracer m a
    -> (a -> m ())
    -> (forall b. (b -> a) -> Tracer m b)
    -> Tracer m a
```

A pattern synonym to deconstruct a `Tracer` into itself, its underlying emitter function, and a contravariant mapping function against itself.

**Fields:**

- `tracer` - returns the tracer unchanged
- `trace` - extracts the `a -> m ()` emitter function
- `contra` - applies contravariant mapping

## Examples

### Pattern Matching

```haskell
{-# LANGUAGE PatternSynonyms #-}
import Data.Tracer.Contrib

example :: Tracer IO String -> IO ()
example (TraceWith t emit mapTrace) = do
    emit "Direct emit"                    -- use emitter directly
    traceWith t "Normal trace"            -- use tracer normally
    let intTracer = mapTrace show         -- map to trace integers
    traceWith intTracer 42
```

### Using Fields as Functions

```haskell
import Data.Tracer.Contrib

example :: Tracer IO String -> IO ()
example t = do
    -- Extract and use the emitter
    let emit = trace t
    emit "Hello"

    -- Create a derived tracer
    let intTracer = contra t show
    traceWith intTracer 123
```

### Composing Transformations

```haskell
import Data.Tracer.Contrib

pipeline :: Tracer IO String -> IO ()
pipeline t = do
    let showTracer = contra t show :: Tracer IO Int
        doubleTracer = contra showTracer (* 2) :: Tracer IO Int

    traceWith doubleTracer 5
    -- Output: "10"
```

### Direct Emitter Access

Sometimes you need the raw `a -> m ()` function:

```haskell
import Data.Tracer.Contrib
import qualified Data.Map.Strict as Map

logMap :: Tracer IO String -> Map.Map String Int -> IO ()
logMap t m = do
    let emit = trace t
    Map.traverseWithKey (\k v -> emit $ k ++ ": " ++ show v) m
    pure ()
```

## Notes

- The pattern is `COMPLETE` - you can use it in exhaustive pattern matches
- `contra t id` is equivalent to `t` (identity mapping)
- The `contra` field provides the same functionality as `contramap` but with a more convenient syntax for partial application
