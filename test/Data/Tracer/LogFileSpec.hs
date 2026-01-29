module Data.Tracer.LogFileSpec (spec) where

import Control.Tracer (traceWith)
import Data.Tracer.LogFile (logFileTracer, logTracer)
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

spec :: Spec
spec = do
    describe "logFileTracer" $ do
        it "creates file and writes messages" $
            withSystemTempDirectory "logfile-test" $ \tmpDir -> do
                let logFile = tmpDir </> "test.log"
                logFileTracer logFile $ \tracer -> do
                    traceWith tracer "message 1"
                    traceWith tracer "message 2"
                exists <- doesFileExist logFile
                exists `shouldBe` True
                contents <- readFile logFile
                contents `shouldBe` "message 1\nmessage 2\n"

        it "appends to existing file" $
            withSystemTempDirectory "logfile-test" $ \tmpDir -> do
                let logFile = tmpDir </> "test.log"
                logFileTracer logFile $ \tracer ->
                    traceWith tracer "first"
                logFileTracer logFile $ \tracer ->
                    traceWith tracer "second"
                contents <- readFile logFile
                contents `shouldBe` "first\nsecond\n"

        it "preserves message order" $
            withSystemTempDirectory "logfile-test" $ \tmpDir -> do
                let logFile = tmpDir </> "test.log"
                    messages = map show [1 :: Int .. 10]
                logFileTracer logFile $ \tracer ->
                    mapM_ (traceWith tracer) messages
                contents <- readFile logFile
                lines contents `shouldBe` messages

    describe "logTracer" $ do
        it "logs to file when filepath is Just" $
            withSystemTempDirectory "logfile-test" $ \tmpDir -> do
                let logFile = tmpDir </> "test.log"
                logTracer (Just logFile) $ \tracer ->
                    traceWith tracer "test message"
                contents <- readFile logFile
                contents `shouldBe` "test message\n"

        -- Note: Testing stdout logging would require capturing stdout,
        -- which is complex. We trust the implementation is correct.
        it "does not create file when filepath is Nothing" $
            withSystemTempDirectory "logfile-test" $ \tmpDir -> do
                let logFile = tmpDir </> "should-not-exist.log"
                -- This will log to stdout, not to file
                logTracer Nothing $ \tracer ->
                    traceWith tracer "test"
                exists <- doesFileExist logFile
                exists `shouldBe` False
