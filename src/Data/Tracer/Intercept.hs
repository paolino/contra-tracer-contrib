{-# LANGUAGE RankNTypes #-}

module Data.Tracer.Intercept
    ( intercept
    ) where

-- \|
-- Module      : Data.Tracer.Intercept
-- Description : Selective forwarding via partial mapping
-- Copyright   : (c) Paolo Veronelli, 2025
-- License     : Apache-2.0
--
-- Provides a combinator for intercepting trace events and selectively
-- forwarding them to a secondary tracer based on a partial mapping function.

import Control.Tracer (Tracer, arrow, emit, traceWith)
import Data.Foldable (for_)

{- | Modify a 'Tracer' to emit to another 'Tracer' based on a partial mapping
function.

The intercepted tracer continues to emit all events to the primary tracer,
but also forwards events to a secondary tracer when the mapping function
returns 'Just'.

@
intercept secondary mapping primary
@

* @secondary@ - receives transformed events when mapping succeeds
* @mapping@ - partial function to transform events
* @primary@ - receives all events unchanged
-}
intercept
    :: (Monad m)
    => Tracer m b
    -- ^ secondary tracer for intercepted events
    -> (a -> Maybe b)
    -- ^ partial mapping function
    -> Tracer m a
    -- ^ primary tracer
    -> Tracer m a
intercept secondary g primary = arrow $ emit $ \a -> do
    for_ (g a) (traceWith secondary)
    traceWith primary a
