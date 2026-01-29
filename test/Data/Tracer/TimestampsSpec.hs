module Data.Tracer.TimestampsSpec (spec) where

import Control.Tracer (arrow, emit, traceWith)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Tracer.Timestamps (addTimestampsTracer)
import Test.Hspec

spec :: Spec
spec = do
    describe "addTimestampsTracer" $ do
        it "prepends timestamp to messages" $ do
            ref <- newIORef ""
            let baseTracer = arrow $ emit $ writeIORef ref
                timestamped = addTimestampsTracer baseTracer
            traceWith timestamped "test message"
            output <- readIORef ref
            -- Should start with [ and contain ] before message
            take 1 output `shouldBe` "["
            "] test message" `shouldSatisfy` (`isInfixOf'` output)

        it "produces valid UTC timestamp format" $ do
            ref <- newIORef ""
            let baseTracer = arrow $ emit $ writeIORef ref
                timestamped = addTimestampsTracer baseTracer
            traceWith timestamped "test"
            output <- readIORef ref
            -- Extract timestamp between [ and ]
            let timestamp = takeWhile (/= ']') (drop 1 output)
            -- Should end with " UTC"
            " UTC" `shouldSatisfy` (`isSuffixOf'` timestamp)
            -- Should contain date-like pattern
            '-' `shouldSatisfy` (`elem` timestamp)
            ':' `shouldSatisfy` (`elem` timestamp)

        it "preserves original message content" $ do
            ref <- newIORef ""
            let baseTracer = arrow $ emit $ writeIORef ref
                timestamped = addTimestampsTracer baseTracer
                testMsg = "This is a test message with special chars: !@#$%"
            traceWith timestamped testMsg
            output <- readIORef ref
            testMsg `shouldSatisfy` (`isInfixOf'` output)

        it "handles empty messages" $ do
            ref <- newIORef ""
            let baseTracer = arrow $ emit $ writeIORef ref
                timestamped = addTimestampsTracer baseTracer
            traceWith timestamped ""
            output <- readIORef ref
            -- Should still have timestamp followed by ] and space
            "] " `shouldSatisfy` (`isInfixOf'` output)

        it "handles multiline messages" $ do
            ref <- newIORef ""
            let baseTracer = arrow $ emit $ writeIORef ref
                timestamped = addTimestampsTracer baseTracer
                testMsg = "line1\nline2\nline3"
            traceWith timestamped testMsg
            output <- readIORef ref
            testMsg `shouldSatisfy` (`isInfixOf'` output)

-- Helper functions since Data.List.isInfixOf and isSuffixOf
-- return Bool not a -> Bool
isInfixOf' :: (Eq a) => [a] -> [a] -> Bool
isInfixOf' needle haystack = any (isPrefixOf' needle) (tails' haystack)

isPrefixOf' :: (Eq a) => [a] -> [a] -> Bool
isPrefixOf' [] _ = True
isPrefixOf' _ [] = False
isPrefixOf' (x : xs) (y : ys) = x == y && isPrefixOf' xs ys

isSuffixOf' :: (Eq a) => [a] -> [a] -> Bool
isSuffixOf' needle haystack =
    isPrefixOf' (reverse needle) (reverse haystack)

tails' :: [a] -> [[a]]
tails' [] = [[]]
tails' xs@(_ : xs') = xs : tails' xs'
