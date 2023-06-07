module Main where

import Control.Monad.Trans.MSF.Except
import Control.Monad.Trans.MSF.Reader
import FRP.BearRiver
import SDL.Event

add :: (Num n, Monad m) => n -> MSF m n n
add n0 = arr (+ n0)
-- embed (add 7) [1,2,3]
-- > [8,9,10]

-- embed (first $ add 7) [(1,1),(2,2),(3,3)]
-- > [(8,1),(9,2),(10,3)]

testSerial :: MSF IO () ()
testSerial = arrM (\() -> getLine)
             >>> (arr id &&& arr reverse)
             >>> arrM print

sumFrom' :: (Num n, Monad m) => n -> MSF m n n
sumFrom' n0 = feedback n0 (arr add2)
             where
               add2 (n, acc) = let n' = n + acc in (n',n')

add2' :: Num b => (b, b) -> (b, b)
add2' (n, acc) = let n' = n + acc in (n',n')               

count' :: (Num n, Monad m) => MSF m () n
count' = arr (const 1) >>> sumFrom' 0

foo :: (Num n, Monad m) => MSF m () n
foo = arr (const 1)

bar :: MSF IO () ()
bar = (arr (const 1) &&& arr (const "Suka!")) 
      >>> arrM print

--ballToRight :: Monad m => MSF (ReaderT GameSettings (ListT m)) () Ball
--ballToRight :: MSF (ReaderT r m) a Integer
ballToRight :: (Monad m, Num r) => p -> MSF (ReaderT r m) () Int
ballToRight n = do
  arr $ const 1
  --arrM asks

--embed (runReaderS myMSF) [("Suchka", ())]  
myMSF :: Monad m => MSF (ReaderT String m) () Int
myMSF = constM ask >>> arr length  

ballToRight' :: (Monad m) => p -> MSF m () Int
ballToRight' n = do
  arr $ const 1
    
  --count >>> arrM (\n -> (n+) <$> asks (const 3))
  --return $ arrM (const 3)
  
sumFrom'' :: (Monad m, Num r, Num a) => p -> ReaderT r m a
sumFrom'' n0 = do
  local (const 3) $ do
    return 3
    -- feedback n0 (arr add2)
    --          where
    --            add2 (n, acc) = let n' = n + acc in (n',n')

-- getMouse :: IO (Float, Float, Bool, Bool)
-- getMouse = do
--   pumpEvents
--   (x,y,btns) <- SDL.getMouseState
--   let left  = ButtonLeft  `elem` btns
--       right = ButtonRight `elem` btns
--   return (fromIntegral x, fromIntegral y, left, right)

getMouse :: MSF IO () Int --(Float, Float, Bool, Bool)
getMouse = do
  --pumpEvents
  arr $ const 1

baz :: MSF IO () ()
baz = (arr (const 1) >>> sumFrom' 0 &&& arr (const "Suka!")) 
      >>> arrM print
  
main :: IO ()
main = do
  embed foo [(),(),()]
  embed (runReaderS myMSF) [("Suchka", ())]
  pure ()
