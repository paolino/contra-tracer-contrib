{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}

module Data.Tracer.TraceWith
    ( pattern TraceWith
    , tracer
    , trace
    , contra
    ) where

-- \|
-- Module      : Data.Tracer.TraceWith
-- Description : Pattern synonym for tracer deconstruction
-- Copyright   : (c) Paolo Veronelli, 2025
-- License     : Apache-2.0
--
-- Provides a pattern synonym to deconstruct a 'Tracer' into its components,
-- enabling direct access to the emitter function and contravariant mapping.

import Control.Arrow ((&&&))
import Control.Tracer (Tracer, traceWith)
import Data.Functor.Contravariant (contramap)

-- without this passage GHC cannot expose a pattern synonym with a rank-n type
newtype Contra m a = Contra (forall b. (b -> a) -> Tracer m b)

mkContra :: (Monad m) => Tracer m a -> Contra m a
mkContra t = Contra (`contramap` t)

mkTrace :: (Monad m) => Tracer m a -> a -> m ()
mkTrace = traceWith

{-# COMPLETE TraceWith #-}

{- | A pattern synonym to deconstruct a 'Tracer' into itself, its underlying
emitter function, and a contravariant mapping function against itself.

This enables multiple ways to work with a tracer simultaneously:

@
example :: Tracer IO String -> IO ()
example (TraceWith t emit mapTrace) = do
    emit "Direct emit"                    -- use the emitter directly
    traceWith t "Normal trace"            -- use the tracer normally
    let intTracer = mapTrace show         -- map to trace integers
    traceWith intTracer 42
@

The record fields can also be used as functions:

* 'tracer' - returns the tracer unchanged
* 'trace' - extracts the @a -> m ()@ emitter function
* 'contra' - applies contravariant mapping
-}
pattern TraceWith
    :: (Monad m)
    => Tracer m a
    -> (a -> m ())
    -> (forall b. (b -> a) -> Tracer m b)
    -> Tracer m a
pattern TraceWith{tracer, trace, contra} <-
    tracer@(mkTrace &&& mkContra -> (trace, Contra contra))
