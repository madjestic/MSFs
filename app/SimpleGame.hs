{-# LANGUAGE OverloadedStrings #-}
module Main where

import           SDL
import           Control.Concurrent
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.MSF
import           Data.MonadicStreamFunction  
import qualified Data.MonadicStreamFunction as MSF
import           Unsafe.Coerce

type DTime = Double

newtype Game = Game
  { tick :: Integer } deriving Show

initGame :: Game
initGame =
  Game { tick = -1 }

game :: MSF (ReaderT DTime (ReaderT Game IO)) () Game
game = arrM (\() -> (lift . lift) updateGame)
  where
    updateGame :: IO Game
    updateGame = do
      -- liftIO $ delay 1000
      -- print "Hello, Game!"
      return $ initGame { tick = 100 }
  
renderOutput :: Renderer -> Game -> IO ()
renderOutput renderer g = do
  liftIO $ delay 1
  events <- SDL.pollEvents
  mp <- getAbsoluteMouseLocation
  let mousePos = (\(V2 x y) -> (unsafeCoerce x,unsafeCoerce y)) (unP mp)
  rendererDrawColor renderer $= uncurry (V4 (fromIntegral $ tick g)) mousePos 255
  clear renderer
  present renderer

animate :: SDL.Window -> MSF (ReaderT DTime (ReaderT Game IO)) () Game -> IO ()  
animate window sf = do
  renderer <- createRenderer window (-1) defaultRenderer
  MSF.reactimate $ input >>> sfIO >>> output renderer
  where
    input    = arr (const (initGame, (0.2, ()))) :: MSF IO  b                  (Game, (DTime, ()))
    sfIO     = runReaderS (runReaderS sf)        :: MSF IO (Game, (DTime, ()))  Game
    output r = arrM (renderOutput r)             :: MSF IO  Game               ()
  
main :: IO ()
main = do
  let
    resX  = 800
    resY  = 600
    title = "Simple Game"

  initializeAll
  window   <- createWindow "Simple Game" defaultWindow

  _ <- setMouseLocationMode RelativeLocation
  _ <- warpMouse (WarpInWindow window) (P (V2 (resX`div`2) (resY`div`2)))
  _ <- cursorVisible $= True

  animate window game
  putStrLn "Exiting Game"  
