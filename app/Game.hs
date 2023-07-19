{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Arrows #-}

module Main where

--import FRP.Rhine
import System.Random              ( randomRIO, randomIO )
import Text.Printf                ( printf )
import Data.MonadicStreamFunction ( MSF, returnA )
import Control.Monad              ( guard )              
import Control.Arrow              ( Arrow(arr), (>>>) ) 
import FRP.Rhine
  ( TimeDomain
  , StdinClock (StdinClock)
  , Millisecond
  , ClSF
  , arrMCl
  , Behavior
  , UTCTime
  , integral
  , integralFrom
  , BehaviorF
  , ExceptT (ExceptT), throwMaybe, VectorSpace (zeroVector)
  , MonadIO
  , returnA, ClSFExcept
  , try
  , once_
  , ExceptT
  , safely
  , SN
  , (@@)
  , (-@-)
  , (>>-^)
  , (>--)
  , (-->)
  , Rhine, waitClock, ResamplingPoint, downsampleMillisecond
  , ResBuf, scheduleMillisecond
  , SeqClock
  , flow
  , fifo
  , fifoUnbounded
  , concurrently
  )

import Data.Vector.Sized as VS

type Ball    = (Double, Double, Double)
type BallVel = (Double, Double, Double)

type EventClock  = StdinClock
type SimClock    = Millisecond 10
type StatusClock = Millisecond 500

statusMsg :: ClSF IO StatusClock Ball ()
statusMsg = arrMCl $ \(x,y,z) -> printf "%.2f %.2f %.2f\n" x y z

freeFall :: Monad m
         => BallVel -- ^ The start velocity
         -> BehaviorF m UTCTime () Ball
freeFall v0 =
  arr (const (0, 0, -9.81)) >>> integralFrom v0 >>> integral

startVel :: ClSF IO StdinClock () BallVel
startVel = arrMCl $ const $ do
  velX <- randomRIO (-10, 10)
  velY <- randomRIO (-10, 10)
  velZ <- randomRIO (  3, 10)
  return (velX, velY, velZ)

waiting :: MonadIO m
        => ClSF (ExceptT BallVel m)
        SimClock (Maybe BallVel) Ball
waiting = throwMaybe >>> arr (const zeroVector)

falling :: Monad m
        => BallVel   -- ^ Initial velocity
        -> ClSF (ExceptT () m)
           SimClock (Maybe BallVel) Ball
falling v0 = proc _ -> do
  pos <- freeFall v0 -< ()
  let (_,_,height) = pos
  throwMaybe       -< guard $ height < 0
  returnA          -< pos

ballModes :: ClSFExcept IO SimClock
             (Maybe BallVel) Ball void
ballModes = do
  v0 <- try waiting
  once_ $ putStrLn "Catch!"
  try $ falling v0
  once_ $ putStrLn "Caught!"
  ballModes

ball :: ClSF IO SimClock (Maybe BallVel) Ball
ball = safely ballModes

userBall = startVel

-- data SN m cl a b where
--   Synchronous :: ( cl ~ In cl, cl ~ Out cl) => ClSF m cl a b -> SN m cl a b
--   Sequential  :: SN m clab a b -> ResBuf m (Out clab) (In clcd) b c -> SN m clcd c d -> SN m (SeqClock m clab clcd) a d
--   Parallel    :: SN m cl1 a b -> SN m cl2 a b -> SN m (ParClock m cl1 cl2) a b

-- data Rhine' m cl a b = Rhine'
--   { sn    :: SN m cl a b
--   , clock :: cl
--   }

startVelRh :: Rhine IO StdinClock () BallVel
startVelRh =  startVel @@ StdinClock

ballRh :: Rhine IO SimClock (Maybe BallVel) Ball
ballRh = ball @@ waitClock

statusRh :: Rhine IO StatusClock Ball ()
statusRh = statusMsg @@ waitClock
  
downsampleSimToStatus :: ResBuf IO SimClock StatusClock Ball Ball
downsampleSimToStatus = downsampleMillisecond >>-^ arr VS.head

simToStatus :: ResamplingPoint IO SimClock StatusClock Ball Ball
simToStatus = downsampleSimToStatus -@- scheduleMillisecond

ballStatusRh :: Rhine IO (SeqClock IO SimClock StatusClock) (Maybe BallVel) ()
ballStatusRh = ballRh >-- simToStatus --> statusRh
  
main :: IO ()
main = flow $
  startVelRh >-- fifoUnbounded -@- concurrently --> ballStatusRh
