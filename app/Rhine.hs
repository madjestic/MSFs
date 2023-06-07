{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}

module Main where

--import FRP.Rhine
import Data.MonadicStreamFunction ( MSF )
import FRP.Rhine (TimeDomain, StdinClock, Millisecond)
import Control.Monad.Trans.Reader

type RunningClock m time tag = MSF m () (time, tag)

class TimeDomain time where
  type Diff time
  difftime :: time -> time -> Diff time

class Clock m cl where
  type Time cl
  type Tag  cl
  initClock :: cl -> RunningClock m (Time cl) (Tag cl)

data TimeInfo cl = TimeInfo
  { sinceTick :: Diff (Time cl)
  , sinceInit :: Diff (Time cl)
  , absolute  :: Time cl
  , tag       :: Tag  cl
  }

type ClSF m cl a b = MSF (ReaderT (TimeInfo cl) m) a b

type EventClock  = StdinClock
type SimClock    = Millisecond 10
type StatusClock = Millisecond 500

main :: IO ()
main = putStrLn "Hello, Rhine!"
