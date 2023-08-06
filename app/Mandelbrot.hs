{-# LANGUAGE OverloadedStrings #-}
module Main where

import           SDL
import           Control.Concurrent
import           Control.Monad (unless, when)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.MSF as TMSF
import           Data.MonadicStreamFunction  
import qualified Data.MonadicStreamFunction as MSF
import           Data.Text (Text)
import           Control.Monad.Trans.MSF.Maybe (exit)
import           Control.Monad.Trans.MSF.Except
import           Foreign.C.Types  
import           Unsafe.Coerce
import           Graphics.Rendering.OpenGL as GL
import           Foreign.Ptr (plusPtr, nullPtr, Ptr)
import           Foreign.Marshal.Array (withArray)  
import           Foreign.Storable (sizeOf)

import LoadShaders

type DTime = Double

data Game = Game
  { tick     :: Integer
  , mpos     :: Point V2 CInt
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
  , mpos     = P (V2 0 0)
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
    gameQuit' = TMSF.get >>= \s -> return $ quitGame s

    gameLoop' :: StateT Game IO Bool
    gameLoop' = do
      handleEvents
        where
          handleEvents :: StateT Game IO Bool
          handleEvents = do
            liftIO $ delay 10
            events <- SDL.pollEvents
            updateKeyboard mapKeyEvents events
            updateMouse events
            let result = any isQuit $ fmap eventPayload events :: Bool
            --get >>= (liftIO . print)
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
                      where
                        inc' :: Integer -> Game -> Game
                        inc' k (Game c m q) =
                          Game
                          { tick      = c + k
                          , mpos      = m
                          , quitGame  = q
                          }
                     
                    exit' :: Bool -> StateT Game IO ()
                    exit' b = modify $ quit' b
                     
                    quit' :: Bool -> Game -> Game
                    quit' b gameLoop' = gameLoop' { quitGame = b }

                updateMouse  :: [Event] -> StateT Game IO ()
                updateMouse = mapM_ processEvent 
                  where
                    processEvent :: Event -> StateT Game IO ()
                    processEvent e =
                      let mk = case eventPayload e of
                            MouseMotionEvent mouseEvent -> Just (mouseMotionEventPos mouseEvent)
                            _ -> Nothing
                      in case mk of
                        Nothing   -> return ()
                        Just vpos -> mmove (unsafeCoerce vpos)
                                     where
                                       mmove :: Point V2 CInt -> StateT Game IO ()
                                       mmove pos = modify $ mmove' pos
                                         where
                                           mmove' :: Point V2 CInt -> Game -> Game
                                           mmove' pos (Game c m q) =
                                             Game
                                             { tick     = c
                                             , mpos     = pos
                                             , quitGame = q }
  
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
                        Just (_, k) -> case lookup k mapping of
                                          Nothing -> return ()
                                          Just k  -> k

-- < Rendering > ----------------------------------------------------------
openWindow :: Text -> (CInt, CInt) -> IO SDL.Window
openWindow title (sizex,sizey) = do
    SDL.initialize [SDL.InitVideo]
    SDL.HintRenderScaleQuality $= SDL.ScaleLinear                    
    do renderQuality <- SDL.get SDL.HintRenderScaleQuality          
       when (renderQuality /= SDL.ScaleLinear) $                    
         putStrLn "Warning: Linear texture filtering not enabled!"

    let config = OpenGLConfig { glColorPrecision     = V4 8 8 8 0
                              , glDepthPrecision     = 24
                              , glStencilPrecision   = 8
                              , glMultisampleSamples = 4
                              , glProfile            = Core Normal 4 5
                              }
     
    window <- SDL.createWindow
            "MFS / SDL / OpenGL Example"
              SDL.defaultWindow
              { SDL.windowInitialSize     = V2 sizex sizey
              , SDL.windowGraphicsContext = OpenGLContext config }
              
    SDL.showWindow window
    _ <- SDL.glCreateContext window
    
    return window

type Drawable   = [Vertex4 Double]
type Pos        = (Double, Double)  
data Shape      = Square Pos Double
                deriving Show

toVertex4 :: Pos -> Vertex4 Double
toVertex4 (k, l)   = Vertex4 k l 0 1

square :: Pos -> Double -> [Pos]
square pos side = [p1, p2, p3,
                   p1, p3, p4]
    where          
        x = fst pos
        y = snd pos
        r = side/2 
        p1 = (x + r, y + r)
        p2 = (x - r, y + r)
        p3 = (x - r, y - r)
        p4 = (x + r, y - r)

toPos :: Shape -> [Pos]
toPos (Square pos side) =  square pos side

data Projection = Planar                
                deriving Show 

type UV         = [TexCoord2 Double] 

toUV :: Projection -> UV
toUV Planar =
  projectPlanar ps
  where
    projectPlanar :: [Pos] -> UV
    projectPlanar = map $ uncurry TexCoord2                                                                   
    ps = [(1.0, 1.0),( 0.0, 1.0),( 0.0, 0.0)
         ,(1.0, 1.0),( 0.0, 0.0),( 1.0, 0.0)] :: [Pos]

toDrawable :: Shape -> Drawable
toDrawable x = map toVertex4 $ toPos x

data Descriptor = Descriptor VertexArrayObject ArrayIndex NumArrayIndices

initResources :: ([Vertex4 Double]) -> Double -> IO Descriptor
initResources (vs) timer = do
    triangles <- genObjectName
    bindVertexArrayObject $= Just triangles

    --
    -- Declaring VBO: vertices
    --
    let vertices = vs
        numVertices = length vertices

    vertexBuffer <- genObjectName
    bindBuffer ArrayBuffer $= Just vertexBuffer
    withArray vs $ \ptr -> do
        let size = fromIntegral (numVertices * sizeOf (head vs))
        bufferData ArrayBuffer $= (size, ptr, StaticDraw)

    let firstIndex = 0
        vPosition = AttribLocation 0
    vertexAttribPointer vPosition $=
        (ToFloat, VertexArrayDescriptor 4 GL.Double 0 (bufferOffset firstIndex))
    vertexAttribArray vPosition $= Enabled

    --
    -- Declaring VBO: UVs
    --
    let uv = toUV Planar

    textureBuffer <- genObjectName
    bindBuffer ArrayBuffer $= Just textureBuffer
    withArray uv $ \ptr -> do
        let size = fromIntegral (numVertices * sizeOf (head uv))
        bufferData ArrayBuffer $= (size, ptr, StaticDraw)

    let uvCoords = AttribLocation 1
    vertexAttribPointer uvCoords $=
        (ToFloat, VertexArrayDescriptor 2 GL.Double 0 (bufferOffset firstIndex))
    vertexAttribArray uvCoords   $= Enabled

    program <- loadShaders [
        ShaderInfo VertexShader (FileSource "shaders/shader.vert"),
        ShaderInfo FragmentShader (FileSource "shaders/shader.frag")]
    currentProgram $= Just program

    -- Set Uniforms
    location <- GL.get (uniformLocation program "fTime")
    uniform location $= (realToFrac timer :: GLfloat)

    return $ Descriptor triangles firstIndex (fromIntegral numVertices)    

bufferOffset :: Integral a => a -> Ptr b
bufferOffset = plusPtr nullPtr . fromIntegral
  
renderOutput' :: Window -> (Game, Maybe Bool) -> IO Bool
renderOutput' _ ( _,Nothing) = quit >> return True
renderOutput' window (g1,_) = do
  let
    drawable = toDrawable (Square (0.0, 0.0) 1.0)
    timer    = 0.01 * (fromIntegral $ tick g1)
  (Descriptor triangles firstIndex numVertices) <- initResources drawable timer
  
  GL.clearColor $= Color4 0 0 0 1
  GL.clear [ColorBuffer]
  bindVertexArrayObject $= Just triangles
  drawArrays Triangles firstIndex numVertices

  SDL.glSwapWindow window >> return False

animate' :: Window
         -> MSF (MaybeT (ReaderT GameSettings (ReaderT DTime (StateT Game IO)))) () Bool
         -> IO ()
animate' window sf = do
  --renderer <- createRenderer window (-1) defaultRenderer
  reactimateB $ input >>> sfIO >>> output window
  quit
  where
    input    = arr (const (0.2, (initSettings, ())))                        -- :: MSF IO b (DTime, (GameSettings, ()))
    sfIO     = runStateS_ (runReaderS (runReaderS (runMaybeS sf))) initGame -- :: MSF IO   (DTime, (GameSettings, ())) (Game, Maybe Bool)
    output w = arrM (renderOutput' w)                                       -- :: MSF IO   (Game, Maybe Bool) Bool

renderOutput :: Renderer -> (Game, Maybe Bool) -> IO Bool
renderOutput _ ( _,Nothing) = quit >> return True
renderOutput renderer (g1,_) = do
  let mousePos = (\(V2 x y) -> (unsafeCoerce x,unsafeCoerce y)) (unP (mpos g1))
  rendererDrawColor renderer $= uncurry (V4 (fromIntegral $ tick g1)) mousePos 255
  SDL.clear renderer
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
  window <- openWindow "Mandelbrot + SDL2/OpenGL" (resX', resY')

  _ <- setMouseLocationMode RelativeLocation
  _ <- warpMouse (WarpInWindow window) (P (V2 (resX'`div`2) (resY'`div`2)))
  _ <- cursorVisible $= True

  --animate window game
  animate' window game
  putStrLn "Exiting Game"
