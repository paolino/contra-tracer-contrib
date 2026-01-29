{- |
Module      : Data.Tracer.LogFile
Description : File and stdout logging with buffering
Copyright   : (c) Paolo Veronelli, 2025
License     : Apache-2.0

Provides tracers for logging to files or stdout with appropriate buffering.
-}
module Data.Tracer.LogFile
    ( logFileTracer
    , logTracer
    ) where

import Control.Tracer (Tracer, arrow, emit)
import System.IO
    ( BufferMode (..)
    , IOMode (..)
    , hPutStrLn
    , hSetBuffering
    , stdout
    , withFile
    )

{- | Create a tracer that logs to a file with line buffering.

The file is opened in append mode and closed when the continuation returns.

@
logFileTracer "app.log" $ \\tracer -> do
    traceWith tracer "Application started"
    runApp tracer
@
-}
logFileTracer
    :: FilePath
    -- ^ path to log file
    -> (Tracer IO String -> IO a)
    -- ^ continuation receiving the tracer
    -> IO a
logFileTracer fp k = do
    withFile fp AppendMode $ \handle -> do
        hSetBuffering handle LineBuffering
        k $ arrow $ emit $ hPutStrLn handle

{- | Create a tracer that logs to a file if a filepath is provided,
otherwise logs to stdout with line buffering.

@
logTracer (Just "app.log") $ \\tracer -> ...  -- logs to file
logTracer Nothing $ \\tracer -> ...           -- logs to stdout
@
-}
logTracer
    :: Maybe FilePath
    -- ^ optional path to log file
    -> (Tracer IO String -> IO a)
    -- ^ continuation receiving the tracer
    -> IO a
logTracer Nothing k = do
    hSetBuffering stdout LineBuffering
    k $ arrow $ emit putStrLn
logTracer (Just fp) k = logFileTracer fp k
