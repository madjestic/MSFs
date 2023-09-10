{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImportQualifiedPost #-}
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
import Foreign (sizeOf, peekArray, castPtr)
import           Foreign.C.Types  
import           Unsafe.Coerce
import           Graphics.Rendering.OpenGL as GL
import           Foreign.Ptr (plusPtr, nullPtr, Ptr)
import           Foreign.Marshal.Array (withArray)  
import           Codec.GlTF as GlTF
import           Codec.GlTF.Mesh as Mesh
import           Text.GLTF.Loader as Gltf
import           Lens.Micro
import qualified Data.Vector as V hiding (head, length)
import           Data.Foldable
import           Data.Word
import           GHC.Float
import           Graphics.Rendering.OpenGL (VertexArrayObject, NumArrayIndices)
import           LoadShaders
import           Data.StateVar as SV
import           Codec.GlTF.Mesh (Mesh(..))
import Load_glTF
import Model_glTF
import Geomancy.Vec4
import Geomancy.Vec3
import Geomancy.Vec2
import RIO.Vector qualified as Vector
import Codec.GlTF.Buffer qualified as Buffer
import RIO.ByteString qualified as ByteString
import Data.Coerce (Coercible, coerce)  

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

-- indices :: [GLenum]
-- indices =
--   [          -- Note that we start from 0!
--     0, 1, 3, -- First Triangle
--     1, 2, 3  -- Second Triangle
--   ]

-- verts :: (Double, Double) -> [GLfloat]
-- verts p0 =
--   [ -- | positions    -- | colors      -- | uv
--     1.0,  1.0, 0.0,   1.0, 0.0, 0.0,   1.0 + tx, 1.0 + ty,
--     1.0, -1.0, 0.0,   0.0, 1.0, 0.0,   1.0 + tx, 0.0 + ty,
--    -1.0, -1.0, 0.0,   0.0, 0.0, 1.0,   0.0 + tx, 0.0 + ty,
--    -1.0,  1.0, 0.0,   0.0, 0.0, 0.0,   0.0 + tx, 1.0 + ty
--   ]
--   where
--     tx = (\ (x,y)-> realToFrac x) p0 :: GLfloat
--     ty = (\ (x,y)-> realToFrac y) p0 :: GLfloat

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

type Drawable   = ([Int], [Vertex3 Double])
type Pos        = (Double, Double)  
data Shape      = Square Pos Double
                deriving Show

toVertex4 :: Pos -> Vertex4 Double
toVertex4 (k, l)   = Vertex4 k l 0 1

toVertex4' :: V3 Double -> Vertex4 Double
toVertex4' (V3 x y z)   = Vertex4 x y z 1

toVertex4'' :: V3 Float -> Vertex4 Double
toVertex4'' (V3 x y z)   = Vertex4 (float2Double x) (float2Double y) (float2Double z) 1.0

toVertex3 :: V3 Float -> Vertex3 Double
toVertex3 (V3 x y z)   = Vertex3 (float2Double x) (float2Double y) (float2Double z)

fromVector :: V.Vector (V3 Float) -> [GLfloat]
fromVector vs = concatMap (\(V3 x y z) -> [x,y,z]) (V.toList vs)

-- square :: Pos -> Double -> [Pos]
-- square pos side = [p1, p2, p3,
--                    p1, p3, p4]
--     where          
--         x = fst pos
--         y = snd pos
--         r = side/2 
--         p1 = (x + r, y + r)
--         p2 = (x - r, y + r)
--         p3 = (x - r, y - r)
--         p4 = (x + r, y - r)

-- toPos :: Shape -> [Pos]
-- toPos (Square pos side) =  square pos side

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

toDescriptor :: FilePath -> IO Descriptor
toDescriptor file = do
  (idx, vs) <- loadGltf'
  initResources vs idx 0

data Descriptor = Descriptor VertexArrayObject NumArrayIndices
  deriving Show

fromVertex3 :: Vertex3 Double -> [GLfloat]
fromVertex3 (Vertex3 x y z) = [double2Float x, double2Float y, double2Float z]

initResources :: [GLfloat] -> [GLenum] -> Double -> IO Descriptor
initResources vs idx z0 =  
  do
    -- | VAO
    triangles <- genObjectName
    bindVertexArrayObject $= Just triangles

    -- | VBO
    vertexBuffer <- genObjectName
    bindBuffer ArrayBuffer $= Just vertexBuffer
    let numVertices = length vs
    withArray vs $ \ptr ->
      do
        let sizev = fromIntegral (numVertices * sizeOf (head vs))
        bufferData ArrayBuffer $= (sizev, ptr, StaticDraw)

    -- | EBO
    elementBuffer <- genObjectName
    bindBuffer ElementArrayBuffer $= Just elementBuffer
    let numIndices = length idx
    withArray idx $ \ptr ->
      do
        let indexSize = fromIntegral $ numIndices * sizeOf (0 :: GLenum)
        bufferData ElementArrayBuffer $= (indexSize, ptr, StaticDraw)
        
    -- | Bind the pointer to the vertex attribute data
    let floatSize  = (fromIntegral $ sizeOf (0.0::GLfloat)) :: GLsizei
        stride     = 8 * floatSize

    -- | Positions
    let vPosition = AttribLocation 0
        posOffset = 0 * floatSize
    vertexAttribPointer vPosition $=
        (ToFloat, VertexArrayDescriptor 3 Float stride (bufferOffset posOffset))
    vertexAttribArray vPosition   $= Enabled

    -- | Colors
    let vaRGBA     = AttribLocation 1
        rgbaOffset = 3 * floatSize
    vertexAttribPointer vaRGBA  $=
        (ToFloat, VertexArrayDescriptor 4 Float stride (bufferOffset rgbaOffset))
    vertexAttribArray vaRGBA    $= Enabled

    -- | UV
    let uvCoords = AttribLocation 2
        uvOffset = 6 * floatSize
    vertexAttribPointer uvCoords  $=
        (ToFloat, VertexArrayDescriptor 2 Float stride (bufferOffset uvOffset))
    vertexAttribArray uvCoords    $= Enabled

    -- || Shaders
    program <- loadShaders [
        ShaderInfo VertexShader   (FileSource "shaders/shader.vert"),
        ShaderInfo FragmentShader (FileSource "shaders/shader.frag")]
    currentProgram $= Just program

    -- || Set Uniforms
    location <- SV.get (uniformLocation program "fTime")
    uniform location $= (realToFrac z0 :: GLfloat)

    -- || Set Transform Matrix
    let tr :: [GLfloat]
        tr =
          [ 1, 0, 0, 0
          , 0, 1, 0, 0
          , 0, 0, 1, 0
          , 0, 0, 0, 1 ]
          
    transform <- GL.newMatrix ColumnMajor tr :: IO (GLmatrix GLfloat)
    location2 <- SV.get (uniformLocation program "transform")
    uniform location2 $= (transform)

    -- || Unload buffers
    bindVertexArrayObject         $= Nothing

    return $ Descriptor triangles (fromIntegral numIndices)

bufferOffset :: Integral a => a -> Ptr b
bufferOffset = plusPtr nullPtr . fromIntegral
  
renderOutput :: Window -> Descriptor -> (Game, Maybe Bool) -> IO Bool
renderOutput _ _ ( _,Nothing) = quit >> return True
renderOutput window d (g1,_) = do
  let
    timer    = 0.01 * (fromIntegral $ tick g1)
    p0 = (0,0) :: (Double, Double)
    z0 = 0     :: Double
  
  GL.clear [ColorBuffer, DepthBuffer]
  
  let (Descriptor triangles numIndices) = d
  bindVertexArrayObject $= Just triangles

  GL.pointSize $= 10.0
  
  drawElements GL.Triangles numIndices GL.UnsignedInt nullPtr  
  glSwapWindow window >> return False

animate :: Window
         -> MSF (MaybeT (ReaderT GameSettings (ReaderT DTime (StateT Game IO)))) () Bool
         -> IO ()
animate window sf = do
  d <- toDescriptor "src/Model.gltf"
  reactimateB $ input >>> sfIO >>> output d window
  quit
  where
    input    = arr (const (0.2, (initSettings, ())))                        :: MSF IO b (DTime, (GameSettings, ()))
    sfIO     = runStateS_ (runReaderS (runReaderS (runMaybeS sf))) initGame :: MSF IO   (DTime, (GameSettings, ())) (Game, Maybe Bool)
    output w d = arrM (renderOutput d w)                                   -- :: MSF IO   (Game, Maybe Bool) Bool

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

  animate window game
  putStrLn "Exiting Game"

loadGltf' :: IO ([GLenum],[GLfloat])
loadGltf' = do
  (root, meshPrimitives) <- loadMeshPrimitives False False "src/Model.gltf"
  let
    (maybeMatTuple, stuff) = head $ V.toList $ head $ V.toList meshPrimitives
    positions = sPositions stuff
    indices   = sIndices   stuff
    idx       = fromIntegral <$> V.toList indices
    attrs     = sAttrs     stuff
    uvs       = vaTexCoord <$> V.toList attrs
    colors    = vaRGBA     <$> V.toList attrs
    normals   = vaNormal   <$> V.toList attrs

  let
    ps = fromVec3' . unPacked <$> V.toList positions
    cs = fromVec4' <$> colors
    ts = fromVec2' <$> uvs
    d = (,,) <$.> ps <*.> cs <*.> ts
    --verts = concatMap (\((x,y,z),(cr,cg,cb,ca),(u,v)) -> [x,y,z,cr,cg,cb,ca,u,v]) (V.toList ((d!!) <$> (fromIntegral <$> indices)))
    verts = concatMap (\((x,y,z),(cr,cg,cb,ca),(u,v)) -> [x,y,z,cr,cg,cb,u,v]) d
  return (idx, verts)

(<$.>) :: (a -> b) -> [a] -> [b]
(<$.>) = fmap

(<*.>) :: [a -> b] -> [a] -> [b]
(<*.>) = zipWith ($)

fromVec2' :: Vec2 -> (Float, Float)
fromVec2' xy = withVec2 (coerce xy) (,)
  
fromVec3' :: Vec3 -> (Float, Float, Float)
fromVec3' xyz = withVec3 (coerce xyz) (,,)

fromVec4' :: Vec4 -> (Float, Float, Float, Float)
fromVec4' xyzw = withVec4 (coerce xyzw) (,,,)

loadGltfFile :: IO (Either Errors Gltf)
loadGltfFile = Gltf.fromJsonFile "src/Model.gltf"

getVertices :: Gltf -> V.Vector (V3 Float)
getVertices gltf = V.concatMap getVertices' (gltf ^. _meshes)
  where getVertices' mesh = V.concatMap (^. _meshPrimitivePositions) (mesh ^. _meshPrimitives)

getIndices :: Gltf -> V.Vector Word16
getIndices gltf = V.concatMap getVertices' (gltf ^. _meshes)
  where getVertices' mesh = V.concatMap (^. _meshPrimitiveIndices) (mesh ^. _meshPrimitives)
