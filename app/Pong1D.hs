{-# LANGUAGE Rank2Types, KindSignatures #-}

module Main where

import Control.Monad.Trans.Class (lift)
import Control.Monad (when, MonadPlus)
import Data.Functor.Classes

import Data.MonadicStreamFunction.Instances.ArrowPlus  
import Control.Monad.Trans.MSF.State
import Control.Monad.Trans.MSF.Maybe
import Control.Monad.Trans.MSF.Except
import Control.Monad.Trans.MSF.Reader
import Control.Monad.Trans.MSF.Writer
import Control.Monad.Trans.Reader
import FRP.BearRiver
    ( embed,
      liftTransS,
      arrM,
      (>>>),
      count,
      Arrow((&&&), arr),
      ArrowPlus((<+>)),
      )
import Data.MonadicStreamFunction.Core
import Data.MonadicStreamFunction.InternalCore
--import Control.Monad.Trans.MSF.List (widthFirst, ListT)
import ListT

runListT :: forall (m :: * -> *) a. ListT m a -> m [a]
runListT = undefined

widthFirst :: (Functor m, Monad m) => MSF (ListT m) a b -> MSF m a [b]
widthFirst msf = widthFirst' [msf]
  where
    widthFirst' msfs = MSF $ \a -> do
      (bs, msfs') <- unzip . concat <$> mapM (toList . flip unMSF a) msfs
      return (bs, widthFirst' msfs')
  
type Game = Ball
type Ball = Int
  
data GameSettings
  =  GameSettings
  { leftPlayerPos  :: Int
  , rightPlayerPos :: Int
  } deriving Show
  
--type GameEnv m = WriterT [String] (ReaderT GameSettings m)
--type GameEnv m = WriterT [String] (ReaderT GameSettings m)
type GameEnv m = ReaderT GameSettings (ListT m)

initGameSettings :: GameSettings  
initGameSettings =
  GameSettings
  { rightPlayerPos = 5
  , leftPlayerPos  = 0 }
  
-- ballToRight :: Monad m => MSF m () Ball
-- ballToRight = count >>> arr (leftPlayerPos +)
-- ballToRight :: Monad m => MSF (GameEnv m) () Ball
-- ballToRight = count >>> arrM ( \n -> (n+) <$> asks leftPlayerPos)
-- ballToRight :: Monad m => MSF (GameEnv m) () Ball
-- ballToRight =
--   count >>> arrM addLeftPlayerPos >>> arrM checkHitR
--   where
--     checkHitR :: Monad m => Int -> GameEnv m Int
--     checkHitR n = do
--         rp <- lift $ asks leftPlayerPos
--         when (rp < n) $ tell ["Ball at " ++ show n]
--         return n
--     addLeftPlayerPos = (\n -> (n+) <$> lift (asks leftPlayerPos))

--ballToRight :: Monad m => MSF (GameEnv m) () Ball
ballToRight :: Monad m => MSF (ReaderT GameSettings (ListT m)) () Ball
ballToRight =
  count >>> arrM (\n -> (n+) <$> asks leftPlayerPos)

-- ballToLeft :: Monad m => MSF m () Ball
-- ballToLeft = count >>> arr (rightPlayerPos -)
-- ballToLeft :: Monad m => MSF (GameEnv m) () Ball
-- ballToLeft = count >>> arrM ( \n -> (n-) <$> lift (asks rightPlayerPos))
-- ballToLeft :: Monad m => MSF (GameEnv m) () Ball
-- ballToLeft =
--   count >>> arrM addRightPlayerPos >>> arrM checkHitR

--   where  checkHitR    :: Monad m => Int -> GameEnv m Int
--          checkHitR n  = do
--            rp <- lift (asks rightPlayerPos)
--            when (rp > n) $ tell [ "Ball at " ++ show n ]
--            return n
--          addRightPlayerPos = (\n -> ((-n)+) <$> lift (asks rightPlayerPos))
ballToLeft :: Monad m => MSF (GameEnv m) () Ball
ballToLeft =
  count >>> arrM (\n -> ((-n)+) <$> asks rightPlayerPos)

hitRight :: Monad m => MSF (GameEnv m) Ball Bool
hitRight = arrM (\n -> (n >=) <$> asks rightPlayerPos)

hitLeft :: Monad m => MSF (GameEnv m) Ball Bool
hitLeft = arrM (\n -> (n <=) <$> asks leftPlayerPos)

-- hitLeft :: Monad m => MSF (GameEnv m) Ball Bool
-- hitLeft = arrM (\n -> (n >=) <$> asks rightPlayerPos)

ballBounceOnce :: (Monad m, MonadPlus m) => MSF (GameEnv m) () Ball
ballBounceOnce = ballUntilRight `catchMaybe` ballLeft

-- ballLeft :: (Monad m, MonadPlus m) => MSF (GameEnv m) () Ball
-- ballLeft = singleBallLeft <+> singleBallLeft
--   where
--     singleBallLeft =
--       count >>>
--       arrM (\n -> (\p -> p - n) <$> lift (asks rightPlayerPos))
ballLeft :: (Monad m, MonadPlus m) => MSF (GameEnv m) () Ball
ballLeft = singleBallLeft <+> singleBallLeft
  where
    singleBallLeft =
      count >>>
      arrM (\n -> (\p -> p - n) <$> asks rightPlayerPos)
  
ballUntilRight :: Monad m => MSF (MaybeT (GameEnv m)) () Ball
ballUntilRight =
  liftTransS (ballToRight
  >>> (arr id &&& hitRight))
  >>> arrM filterHit
  where
    filterHit (b,c) = MaybeT $ return $
      if c then Nothing else Just b

ballUntilLeft :: Monad m => MSF (MaybeT (GameEnv m)) () Ball
ballUntilLeft =
  liftTransS (ballToLeft
  >>> (arr id &&& hitLeft))
  >>> arrM filterHit
  where
    filterHit (b,c) = MaybeT $ return $
      if c then Nothing else Just b

-- game :: Monad m => MSF (GameEnv m) () Ball
-- game = ballUntilRight `catchMaybe` (ballUntilLeft `catchMaybe` game)

game :: Monad m => MSF (GameEnv (StateT Integer m)) () Ball
game =
  ballToRight `untilMaybe` hitRight
  `catchMaybe`
  ballToLeft  `untilMaybe` hitLeft
  `catchMaybe`
  (lift (lift incOneRound) `andThen` game)

incOneRound :: Monad m => StateT Integer m ()
incOneRound = modify (+1)

andThen :: Monad m => m () -> MSF m a b -> MSF m a b
andThen a b = performOnFirstSample (a >> return b)

testMSF :: Monad m => MSF (GameEnv m) () (Ball, Bool)
testMSF = ballToRight >>> (arr id &&& hitRight)

-- testMSF' :: Monad m => GameSettings -> MSF (GameEnv m) () (Ball, Bool)
-- testMSF' s game = game >>>= ballToRight >>> (arr id &&& hitRight)

-- embed (runReaderS(runWriterS testMSF)) [(GameSettings 0 3, ())]
-- embed (runReaderS(runWriterS game)) $ replicate 23 (GameSettings 0 5, ())

mainMSF :: MSF IO () ()
mainMSF =
  runStateS_ (widthFirst parallelGame) 0 >>> arrM print
  where
    parallelGame :: Monad m => MSF (ListT (StateT Integer m)) () (Ball, Ball)
    parallelGame =
          runReaderS_ game (GameSettings 17 20)
      &&& runReaderS_ game (GameSettings 4  10)

mainMSF1 :: MSF IO () ()
mainMSF1 =
  runStateS_ (widthFirst parallelGame) 0 >>> arrM print
  where
    parallelGame :: Monad m => MSF (ListT (StateT Integer m)) () (Ball)
    parallelGame = runReaderS_ game (GameSettings 17 20)
      
main :: IO ()
main = do
  --x <- embed (runReaderS(runWriterS game)) $ replicate 23 (GameSettings 0 5, ())
  -- print x
  _ <- embed mainMSF  $ replicate 15 ()
  _ <- embed mainMSF1 $ replicate 15 ()
  return ()
