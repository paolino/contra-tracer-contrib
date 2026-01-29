{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Tracer.TraceWithSpec (spec) where

import Control.Tracer (Tracer, arrow, emit, traceWith)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Tracer.TraceWith
    ( contra
    , trace
    , tracer
    , pattern TraceWith
    )
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
    describe "TraceWith pattern" $ do
        it "extracts tracer unchanged" $ do
            ref <- newIORef ""
            let original :: Tracer IO String
                original = arrow $ emit $ writeIORef ref
                TraceWith extracted _ _ = original
            traceWith extracted "test"
            result <- readIORef ref
            result `shouldBe` "test"

        it "extracts trace function" $ do
            ref <- newIORef ""
            let original :: Tracer IO String
                original = arrow $ emit $ writeIORef ref
                TraceWith _ emitFn _ = original
            emitFn "direct emit"
            result <- readIORef ref
            result `shouldBe` "direct emit"

        it "extracts contra function" $ do
            ref <- newIORef (0 :: Int)
            let original :: Tracer IO Int
                original = arrow $ emit $ writeIORef ref
                TraceWith _ _ mapFn = original
                stringTracer = mapFn length
            traceWith stringTracer "hello"
            result <- readIORef ref
            result `shouldBe` 5

    describe "tracer field" $ do
        it "returns the tracer unchanged" $
            property $
                \(s :: String) -> ioProperty $ do
                    ref <- newIORef ""
                    let original :: Tracer IO String
                        original = arrow $ emit $ writeIORef ref
                    traceWith (tracer original) s
                    result <- readIORef ref
                    return $ result === s

    describe "trace field" $ do
        it "extracts the emitter function" $
            property $
                \(s :: String) -> ioProperty $ do
                    ref <- newIORef ""
                    let original :: Tracer IO String
                        original = arrow $ emit $ writeIORef ref
                    trace original s
                    result <- readIORef ref
                    return $ result === s

    describe "contra field" $ do
        it "applies contravariant mapping" $
            property $
                \(n :: Int) -> ioProperty $ do
                    ref <- newIORef ""
                    let original :: Tracer IO String
                        original = arrow $ emit $ writeIORef ref
                        intTracer = contra original show
                    traceWith intTracer n
                    result <- readIORef ref
                    return $ result === show n

        it "preserves behavior with id mapping" $
            property $
                \(s :: String) -> ioProperty $ do
                    ref <- newIORef ""
                    let original :: Tracer IO String
                        original = arrow $ emit $ writeIORef ref
                        same = contra original id
                    traceWith same s
                    result <- readIORef ref
                    return $ result === s

        it "composes correctly" $ do
            ref <- newIORef ""
            let original :: Tracer IO String
                original = arrow $ emit $ writeIORef ref
                showTracer :: Tracer IO Int
                showTracer = contra original show
                doubleTracer :: Tracer IO Int
                doubleTracer = contra showTracer (* 2)
            traceWith doubleTracer 5
            result <- readIORef ref
            result `shouldBe` "10"
