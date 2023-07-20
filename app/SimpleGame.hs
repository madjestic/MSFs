{-# LANGUAGE OverloadedStrings #-}
module Main where

import           SDL hiding (get)
import           Control.Concurrent
import           Control.Monad (unless)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.MSF
import           Data.MonadicStreamFunction  
import qualified Data.MonadicStreamFunction as MSF
import           Control.Monad.Trans.MSF.Maybe (exit)
import           Control.Monad.Trans.MSF.Except  
import           Unsafe.Coerce

type DTime = Double

data Game = Game
  { tick     :: Integer
  , quitGame :: Bool
  } deriving Show

data GameSettings = GameSettings
  { resX :: Int 
  , resY :: Int 
  } deriving Show

initGame :: Game
initGame =
  Game
  { tick     = -1
  , quitGame = False
  }

initSettings :: GameSettings
initSettings = GameSettings
  {
    resX = 800
  , resY = 600
  }

game :: MSF (MaybeT (ReaderT GameSettings (ReaderT Double (StateT Game IO)))) () Bool
game = gameLoop `untilMaybe` gameQuit `catchMaybe` exit
  where
    gameLoop = arrM (\_ -> (lift . lift . lift) gameLoop')
    gameQuit = arrM (\_ -> (lift . lift . lift) gameQuit')

    gameQuit' :: StateT Game IO Bool
    gameQuit' = get >>= \s -> return $ quitGame s

    gameLoop' :: StateT Game IO Bool
    gameLoop' = do
      handleEvents
        where
          handleEvents :: StateT Game IO Bool
          handleEvents = do
            liftIO $ delay 1000
            events <- SDL.pollEvents
            updateKeyboard mapKeyEvents events
            --updateMouse
            let result = any isQuit $ fmap eventPayload events :: Bool
            get >>= (liftIO . print)
            return result
              where
                isQuit :: EventPayload -> Bool
                isQuit ev =
                  case ev of
                    KeyboardEvent keyboardEvent -> 
                      keyboardEventKeyMotion keyboardEvent                  == Pressed
                      && keysymScancode (keyboardEventKeysym keyboardEvent) == ScancodeQ
                    QuitEvent -> True
                    _         -> False
                
                mapKeyEvents :: [(Scancode, StateT Game IO ())]
                mapKeyEvents =
                  [ (ScancodeW, inc   10)
                  , (ScancodeS, inc (-10))
                  , (ScancodeQ, exit' True) ]
                  where
                    inc :: Integer -> StateT Game IO ()
                    inc n = modify $ inc' n
                     
                    inc' :: Integer -> Game -> Game
                    inc' k (Game c q) =
                      Game
                      { tick      = c + k
                      , quitGame  = q
                      }
                     
                    exit' :: Bool -> StateT Game IO ()
                    exit' b = modify $ quit' b
                     
                    quit' :: Bool -> Game -> Game
                    quit' b gameLoop' = gameLoop' { quitGame = b }
             
                updateKeyboard :: (Monad m) => [(Scancode, m ())] -> [Event] -> m ()
                updateKeyboard ns = mapM_ (processEvent ns)
                  where
                    processEvent :: (Monad m) => [(Scancode , m ())] -> Event -> m ()
                    processEvent mapping e =
                      let mk = case eventPayload e of
                                 KeyboardEvent keyboardEvent -> Just
                                   ( keyboardEventKeyMotion keyboardEvent == Pressed
                                   , keysymScancode (keyboardEventKeysym keyboardEvent))
                                 _ -> Nothing
                      in case mk of
                           Nothing     -> return ()
                           Just (e', k) -> case lookup k mapping of
                                            Nothing -> return ()
                                            Just k  -> k

renderOutput :: Renderer -> (Game, Maybe Bool) -> IO Bool
renderOutput renderer ( _,Nothing) = quit >> return True
renderOutput renderer (g1,_) = do
  events <- SDL.pollEvents
  mp <- getAbsoluteMouseLocation
  let mousePos = (\(V2 x y) -> (unsafeCoerce x,unsafeCoerce y)) (unP mp)
  rendererDrawColor renderer $= uncurry (V4 (fromIntegral $ tick g1)) mousePos 255
  clear renderer
  present renderer >> return False
  
animate :: Window
        -> MSF (MaybeT (ReaderT GameSettings (ReaderT DTime (StateT Game IO)))) () Bool
        -> IO ()
animate window sf = do
  renderer <- createRenderer window (-1) defaultRenderer
  reactimateB $ input >>> sfIO >>> output renderer
  quit
  where
    input    = arr (const (0.2, (initSettings, ())))                        :: MSF IO b (DTime, (GameSettings, ()))
    sfIO     = runStateS_ (runReaderS (runReaderS (runMaybeS sf))) initGame :: MSF IO   (DTime, (GameSettings, ())) (Game, Maybe Bool)
    output r = arrM (renderOutput r)                                        :: MSF IO   (Game, Maybe Bool) Bool

main :: IO ()
main = do
  
  let (resX', resY') =
        (\opts ->
           ( unsafeCoerce $ fromIntegral $ resX opts
           , unsafeCoerce $ fromIntegral $ resY opts))
        initSettings
  
  initializeAll
  window   <- createWindow "Simple Game" defaultWindow

  _ <- setMouseLocationMode RelativeLocation
  _ <- warpMouse (WarpInWindow window) (P (V2 (resX'`div`2) (resY'`div`2)))
  _ <- cursorVisible $= True

  animate window game
  putStrLn "Exiting Game"
