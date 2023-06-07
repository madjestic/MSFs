{-# LANGUAGE OverloadedStrings #-}
module Main where

import           SDL
import           Control.Concurrent
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.MSF
import           Data.MonadicStreamFunction  
import qualified Data.MonadicStreamFunction as MSF
  
-- import           Debug.Trace
type DTime = Double

newtype Game = Game
  { tick :: Integer } deriving Show

initGame :: Game
initGame =
  Game { tick = -1 }

game :: MSF (ReaderT DTime (ReaderT Game IO)) () Game
game = arrM (\() -> (lift . lift) updateGame)

updateGame :: IO Game
updateGame = do
  print "updateGame"
  return $ initGame { tick = 1 }
  
render :: Game -> IO ()
render g = do
  print g
  threadDelay 1000000
  return ()

animate :: MSF (ReaderT DTime (ReaderT Game IO)) () Game -> IO ()  
animate sf =
  MSF.reactimate $ sense >>> sfIO >>> actuate
  where
    sense   = arr (const (initGame, (0.2, ()))) :: MSF IO  b                  (Game, (DTime, ()))
    sfIO    = runReaderS (runReaderS sf)        :: MSF IO (Game, (DTime, ()))  Game
    actuate = arrM render                       :: MSF IO  Game               ()
  
main :: IO ()
main = do
  let
    resX = 800
    resY = 600

  initializeAll
  window   <- createWindow "Simple Game" defaultWindow

  _ <- setMouseLocationMode RelativeLocation
  _ <- warpMouse (WarpInWindow window) (P (V2 (resX`div`2) (resY`div`2)))
  _ <- cursorVisible $= True

  renderer <- createRenderer window (-1) defaultRenderer
  animate game
  putStrLn "Exiting Game"
  
