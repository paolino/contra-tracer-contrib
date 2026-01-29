{-# LANGUAGE CPP #-}

{- |
Module      : Data.Tracer.Internal
Description : Internal helpers for contra-tracer compatibility
Copyright   : (c) Paolo Veronelli, 2025
License     : Apache-2.0

Provides compatibility layer between Hackage's contra-tracer (arrow-based)
and IOHK's CHaP fork (simple function-based).

Use the @iohk@ cabal flag when building with CHaP's contra-tracer.
-}
module Data.Tracer.Internal
    ( mkTracer
    ) where

#ifdef IOHK_CONTRA_TRACER
-- CHaP's contra-tracer: Tracer wraps (a -> m ())
import Control.Tracer (Tracer (Tracer))

-- | Create a tracer from a function
mkTracer :: (a -> m ()) -> Tracer m a
mkTracer = Tracer
#else
-- Hackage's contra-tracer: Tracer wraps TracerA
import Control.Tracer (Tracer, arrow, emit)

-- | Create a tracer from a function
mkTracer :: (a -> IO ()) -> Tracer IO a
mkTracer f = arrow $ emit f
#endif
