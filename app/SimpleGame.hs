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
import           Unsafe.Coerce

type DTime = Double

data Game = Game
  { tick     :: Integer
  , quitGame :: Bool
  } deriving Show

data Options = Options
  { resX :: Int 
  , resY :: Int 
  } deriving Show

initGame :: Game
initGame =
  Game
  { tick     = -1
  , quitGame = False
  }

initOpts :: Options
initOpts = Options
  {
    resX = 800
  , resY = 600
  }

--------------------------------------------------------------------------
-- Input --
--------------------------------------------------------------------------
isQuit :: EventPayload -> Bool
isQuit ev =
  case ev of
    KeyboardEvent keyboardEvent -> 
      keyboardEventKeyMotion keyboardEvent                  == Pressed
      && keysymScancode (keyboardEventKeysym keyboardEvent) == ScancodeQ
    QuitEvent -> True
    _         -> False

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

updateKeyboard :: (Monad m) => [(Scancode, m ())] -> [Event] -> m ()
updateKeyboard ns = mapM_ (processEvent ns)

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
quit' b g = g { quitGame = b}


mapKeyEvents
  :: [(Scancode, StateT Game IO ())]
mapKeyEvents =
  [
    (ScancodeW, inc   10)
  , (ScancodeS, inc (-10))
  , (ScancodeQ, exit' True)
  ]

handleEvents :: StateT Game IO Bool
handleEvents = do
  events <- SDL.pollEvents
  updateKeyboard mapKeyEvents events
  --updateMouse
  let result = any isQuit $ fmap eventPayload events :: Bool
  return result

--------------------------------------------------------------------------

-- gameLoop' :: MSF (MaybeT (ReaderT Options (ReaderT DTime (StateT Game IO)))) () Game
-- gameLoop' = -- constM (MaybeT $ return Nothing) -- $ foo-- $ arrM (\t -> (lift . lift) gameLoop')
--   --arrM (\t -> (lift . lift . lift) gameLoop')
--   --arrM (\t -> (lift . lift) gameLoop')
--   --arrM (\t -> (lift) gameLoop')
--   arrM (\t -> gameLoop')
--   where
--     --foo = do undefined
--     --gameLoop' ::                  ReaderT DTime (StateT Game IO)  Game
--     --gameLoop' :: ReaderT Options (ReaderT DTime (StateT Game IO)) Game
--     --gameLoop' :: ()
--     gameLoop'  = do undefined
--       -- quitGame' <- handleEvents
--       -- get >>= (liftIO . print)
--       -- return $ initGame { tick = 0 }

gameLoopLift1 :: MSF (MaybeT (ReaderT Options (ReaderT DTime (StateT Game IO)))) () Game
gameLoopLift1 = arrM (\t -> (lift) updateGame)
  where
    updateGame :: ReaderT Options (ReaderT DTime (StateT Game IO))  Game
    updateGame = undefined

gameLoopLift2 :: MSF (MaybeT (ReaderT Options (ReaderT DTime (StateT Game IO)))) () Game
gameLoopLift2 = arrM (\t -> (lift . lift) updateGame)
  where
    updateGame :: ReaderT DTime (StateT Game IO)  Game
    updateGame = undefined

gameLoopLift3 :: MSF (MaybeT (ReaderT Options (ReaderT DTime (StateT Game IO)))) () Game
gameLoopLift3 = arrM (\t -> (lift . lift . lift) updateGame)
  where
    updateGame :: (StateT Game IO)  Game
    updateGame = undefined

gameLoopLift4 :: MSF (MaybeT (ReaderT Options (ReaderT DTime (StateT Game IO)))) () Game
gameLoopLift4 = constM $ MaybeT $ return Nothing

gameLoopLift5 :: MSF (MaybeT (ReaderT Options (ReaderT DTime (StateT Game IO)))) () Game
gameLoopLift5 = do
  let isExit = True
  case isExit of
    False -> constM $ MaybeT $ return Nothing
    True  -> arrM (\t -> (lift . lift) updateGame)
      where
        updateGame = do
          -- liftIO $ delay 1000
          -- print "Hello, Game!"
          return $ initGame { tick = -1 }

-- gameLoop :: MSF (MaybeT (ReaderT Options (ReaderT DTime (StateT Game IO)))) () Game
-- gameLoop = arrM (\t -> (lift . lift) updateGame)
--   where
--     updateGame = do
--       -- liftIO $ delay 1000
--       -- print "Hello, Game!"
--       return $ initGame { tick = 100 }

gameLoop :: MSF (MaybeT (ReaderT Options (ReaderT DTime (StateT Game IO)))) () Game
gameLoop = do
  let isExit = True
  case isExit of
    True  -> constM $ MaybeT $ return Nothing
    False -> arrM (\t -> (lift . lift) updateGame)
      where
        updateGame = do
          -- liftIO $ delay 1000
          -- print "Hello, Game!"
          return $ initGame { tick = 100 }
  
renderOutput :: Renderer -> (Game, Maybe Game) -> IO ()
renderOutput renderer (_,Nothing) = quit
renderOutput renderer (_,Just g1) = do
  liftIO $ delay 1
  events <- SDL.pollEvents
  mp <- getAbsoluteMouseLocation
  let mousePos = (\(V2 x y) -> (unsafeCoerce x,unsafeCoerce y)) (unP mp)
  rendererDrawColor renderer $= uncurry (V4 (fromIntegral $ tick g1)) mousePos 255
  clear renderer
  present renderer

animate :: SDL.Window
        -> MSF (MaybeT (ReaderT Options (ReaderT DTime (StateT Game IO)))) () Game
        -> IO ()  
animate window sf = do
  renderer <- createRenderer window (-1) defaultRenderer
  MSF.reactimate $ input >>> sfIO >>> output renderer
  where
    input    = arr (const (initGame, (0.2, (initOpts, ()))))            :: MSF IO b (Game, (DTime, (Options, ())))
    sfIO     = runStateS (runReaderS (runReaderS (runMaybeS gameLoop))) :: MSF IO   (Game, (DTime, (Options, ()))) (Game, Maybe Game)
    output r = arrM (renderOutput r)                                    :: MSF IO   (Game, Maybe Game) ()

main :: IO ()
main = do
  
  let (resX', resY') =
        (\opts ->
           ( unsafeCoerce $ fromIntegral $ resX opts
           , unsafeCoerce $ fromIntegral $ resY opts))
        initOpts
  
  initializeAll
  window   <- createWindow "Simple Game" defaultWindow

  _ <- setMouseLocationMode RelativeLocation
  _ <- warpMouse (WarpInWindow window) (P (V2 (resX'`div`2) (resY'`div`2)))
  _ <- cursorVisible $= True

  animate window gameLoop
  putStrLn "Exiting Game"
