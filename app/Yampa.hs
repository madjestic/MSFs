{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad.Trans.MSF.Reader
import Control.Monad (when, MonadPlus)
import Data.Functor.Classes

import Data.MonadicStreamFunction.Core
import Data.MonadicStreamFunction.InternalCore
import Data.MonadicStreamFunction.Util
import FRP.BearRiver (VectorSpace(zeroVector), (*^))
import qualified Data.MonadicStreamFunction as MSF
import Data.Functor.Identity (Identity(runIdentity))

type SF a b    = MSF ClockInfo a b
type DTime     = Double
type ClockInfo = Reader DTime

--integral :: VectorSpace a Double => SF a a
integral :: VectorSpace a Double => MSF ClockInfo a a
integral = eulerSteps >>> sumFrom zeroVector
  where eulerSteps = arrM $ \x -> asks (*^x)

--reactimate :: forall a b . IO (DTime, a) -> (b -> IO ()) -> SF a b -> IO ()
reactimate :: forall a b . IO (DTime, a) -> (b -> IO ()) -> MSF ClockInfo a b -> IO ()
reactimate sense actuate sf =
    MSF.reactimate $ senseSF >>> sfIO >>> actuateSF
  where
    senseSF :: MSF IO () (DTime, a)
    senseSF = arrM (\() -> sense)
    
    sfIO :: MSF IO (DTime, a) b
    sfIO = morphS (return . runIdentity) (runReaderS sf)

    actuateSF :: MSF IO b ()
    actuateSF = arrM actuate

twiceAsFast :: Monad m
            => MSF (ReaderT DTime m) a b
            -> MSF (ReaderT DTime m) a b
twiceAsFast = morphS (withReaderT (*2))

twiceAsFast' :: (Monad m, Fractional r)
             => MSF (ReaderT r (ReaderT r m)) a b
             -> MSF (ReaderT r m) a b
twiceAsFast' msf = MSF $ \a ->  do
  dt <- ask
  (_,msf1) <- runReaderT (unMSF msf a ) (dt/2)
  (b,msf2) <- runReaderT (unMSF msf1 a) (dt/2)
  return (b, twiceAsFast' msf2)

type Signal a = MStream (ReaderT DTime IO) a

main :: IO ()
main = undefined
