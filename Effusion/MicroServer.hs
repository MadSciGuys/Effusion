{-|
Module      : Effusion.MicroServer
Description : Framework for Quickly Developing Simple Server Software
Copyright   : Travis Whitaker 2014-2015
License     : All rights reserved.
Maintainer  : twhitak@its.jnj.com
Stability   : Provisional
Portability : POSIX

Effusion.MicroServer provides small "microserver" threads for common concurrent workloads
that can be composed to quicly build and deploy simple server software.
-}

{-# LANGUAGE OverloadedStrings, LambdaCase #-}

module Effusion.MicroServer (

    Done
   ,microInit
   ,microCluster
   ,metaMicroServer

    -- * Loggers
   ,Log(..)
   ,mvarLazyLogger
   ,mvarEagerLogger
   ,chanLazyLogger
   ,chanEagerLogger
   ,mvarLazyWatcher
   ,mvarEagerWatcher

   -- * Pipes
--  ,mvarPipe
--  ,chanPipe

) where

import Control.Concurrent (ThreadId, forkFinally)
import Control.Concurrent.MVar (MVar, newEmptyMVar, takeMVar, putMVar, readMVar)
import Control.Concurrent.Chan (Chan, dupChan, readChan)

import qualified Data.ByteString.Char8 as C (ByteString, hPut, pack)

import System.IO (Handle, hFlush)

-- | Dummy value to indicate microserver termination.
data Done = Done

-- | Fork a microserver. Microservers are expected to handle their own exceptions; as a rule
--   exceptions are not thrown up to the main thread. The 'ThreadId' and an 'MVar' that can be used
--   to detect microserver termination are returned.
microInit :: IO () -> IO (ThreadId, MVar Done)
microInit ms = do
    mvar <- newEmptyMVar
    msid <- forkFinally ms (\_ -> putMVar mvar Done)
    return (msid, mvar)

-- | Run a list of microservers concurrently and wait for them to terminate.
microCluster :: [IO ()] -> IO ()
microCluster ms = mapM microInit ms >>= mapM_ (takeMVar . snd)

-- | A microserver that runs a group of microservers.
metaMicroServer :: [IO ()] -> IO (ThreadId, MVar Done)
metaMicroServer = microInit . microCluster

-- | Type for messages sent to logging microservers.
data Log = Record C.ByteString | Halt

-- | Lazily write data from the provided 'MVar' to the provided 'Handle'.
mvarLazyLogger :: Handle -> MVar Log -> IO ()
mvarLazyLogger h m = loop
    where loop = takeMVar m >>= \case
            Record x -> C.hPut h x >> loop
            Halt     -> return ()

-- | Eagerly write data from the provided 'MVar' to the provided 'Handle'.
mvarEagerLogger :: Handle -> MVar Log -> IO ()
mvarEagerLogger h m = loop
    where loop = takeMVar m >>= \case
            Record x -> C.hPut h x >> hFlush h >> loop
            Halt     -> return ()

-- | Lazily write data from the provided 'Chan' to the provided 'Handle'. The 'Chan' is duplicated
--   first, so multiple 'chanLazyLogger's can listen to the same source 'Chan'.
chanLazyLogger :: Handle -> Chan Log -> IO ()
chanLazyLogger h c = dupChan c >>= loop
    where loop c' = readChan c' >>= \case
            Record x -> C.hPut h x >> loop c'
            Halt     -> return ()


-- | Eagerly write data from the provided 'Chan' to the provided 'Handle'. The 'Chan' is duplicated
--   first, so multiple 'chanLazyLogger's can listen to the same source 'Chan'.
chanEagerLogger :: Handle -> Chan Log -> IO ()
chanEagerLogger h c = dupChan c >>= loop
    where loop c' = readChan c' >>= \case
            Record x -> C.hPut h x >> hFlush h >> loop c'
            Halt     -> return ()

-- | Lazily write data from the provided 'MVar Log' to the provided 'Handle' when trigered by a
--   trigger 'MVar'. Data is never taken from the 'MVar Log', multiple 'mvarLazyWatcher's can
--   listen to the same source 'MVar Log'.
mvarLazyWatcher :: Handle -> MVar Log -> MVar a -> IO ()
mvarLazyWatcher h m t = loop
    where loop = takeMVar t >> readMVar m >>= \case
            Record x -> C.hPut h x >> loop
            Halt     -> return ()

-- | Eagerly write data from the provided 'MVar Log' to the provided 'Handle' when trigered by a
--   trigger 'MVar'. Data is never taken from the 'MVar Log', multiple 'mvarLazyWatcher's can
--   listen to the same source 'MVar Log'.
mvarEagerWatcher :: Handle -> MVar Log -> MVar a -> IO ()
mvarEagerWatcher h m t = loop
    where loop = takeMVar t >> readMVar m >>= \case
            Record x -> C.hPut h x >> hFlush h >> loop
            Halt     -> return ()
