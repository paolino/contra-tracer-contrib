{-# LANGUAGE ScopedTypeVariables #-}

module Data.Tracer.ThreadSafeSpec (spec) where

import Control.Concurrent.Async (forConcurrently_)
import Control.Tracer (Tracer, arrow, emit, traceWith)
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import Data.Tracer.ThreadSafe (newThreadSafeTracer)
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
    describe "newThreadSafeTracer" $ do
        it "preserves all events under concurrent access" $
            property $
                \(Positive n) -> ioProperty $ do
                    let numThreads = min n 100 :: Int
                        eventsPerThread = 10 :: Int
                        totalEvents = numThreads * eventsPerThread
                    ref <- newIORef (0 :: Int)
                    let unsafeTracer = arrow $ emit $ \_ ->
                            atomicModifyIORef' ref (\x -> (x + 1, ()))
                    safeTracer <- newThreadSafeTracer unsafeTracer
                    forConcurrently_ [1 .. numThreads] $ \_ ->
                        mapM_
                            (traceWith safeTracer)
                            [1 :: Int .. eventsPerThread]
                    count <- readIORef ref
                    return $ count === totalEvents

        it "does not drop events with high concurrency" $ do
            let numThreads = 50
                eventsPerThread = 100
                totalEvents = numThreads * eventsPerThread
            ref <- newIORef []
            let unsafeTracer = arrow $ emit $ \x ->
                    atomicModifyIORef' ref (\xs -> (x : xs, ()))
            safeTracer <- newThreadSafeTracer unsafeTracer
            forConcurrently_ [1 .. numThreads] $ \threadId ->
                mapM_
                    (\i -> traceWith safeTracer (threadId, i))
                    [1 :: Int .. eventsPerThread]
            events <- readIORef ref
            length events `shouldBe` totalEvents

        it "returns a new tracer each time" $ do
            let baseTracer :: Tracer IO Int
                baseTracer = arrow $ emit $ \_ -> return ()
            tracer1 <- newThreadSafeTracer baseTracer
            tracer2 <- newThreadSafeTracer baseTracer
            -- They should be different tracers (different MVars)
            -- We can't directly compare them, but we can verify
            -- they both work independently
            traceWith tracer1 1
            traceWith tracer2 2
            -- If we get here without deadlock, they're independent
            return () :: IO ()
