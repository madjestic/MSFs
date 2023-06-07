{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
--{-# LANGUAGE RecordWildCards #-}

module Live where

import Data.Data
import Control.Concurrent.MVar

import LiveCoding

stepProgramMVar
  :: MVar (LiveProgram IO)
  -> IO ()
stepProgramMVar var = do
  currentProgram <- takeMVar var
  nextProgram <- stepProgram currentProgram
  putMVar var nextProgram

update :: MVar (LiveProgram IO)
       ->       LiveProgram IO
       -> IO ()
update var newProg = do
  oldProg <- takeMVar var
  putMVar var $ hotCodeSwap newProg oldProg

main :: IO ()
main = undefined
