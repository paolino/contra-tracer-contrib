{- |
Module      : Data.Tracer.Internal
Description : Internal helpers for tracer construction
Copyright   : (c) Paolo Veronelli, 2025
License     : Apache-2.0

Provides a helper function for creating tracers that works with
contra-tracer's arrow-based API.
-}
module Data.Tracer.Internal
    ( mkTracer
    ) where

import Control.Tracer (Tracer, arrow, emit)

-- | Create a tracer from a function
mkTracer :: (a -> IO ()) -> Tracer IO a
mkTracer f = arrow $ emit f
