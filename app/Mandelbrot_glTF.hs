{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImportQualifiedPost #-}
module Main where

import           SDL hiding (Texture)
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
import           Text.GLTF.Loader as Gltf hiding (Texture)
import           Lens.Micro
import qualified Data.Vector as V hiding (head, length)
import           Data.Foldable as DF
import           Data.Word
import           GHC.Float
import           Graphics.Rendering.OpenGL (VertexArrayObject, NumArrayIndices, DataType (Double), TextureObject (TextureObject))
import           Data.StateVar as SV
import           Codec.GlTF.Mesh (Mesh(..))
import Geomancy.Vec4
import Geomancy.Vec3
import Geomancy.Vec2
import RIO.Vector qualified as Vector
import Codec.GlTF.Buffer qualified as Buffer
import RIO.ByteString qualified as ByteString
import Data.Coerce (Coercible, coerce)
import Data.UUID
import Linear.Projection         as LP        (infinitePerspective)
import Data.Maybe (fromMaybe)
import Data.Set as DS ( fromList, toList )

import Load_glTF (loadMeshPrimitives)
import Model_glTF

import Graphics.RedViz.Texture as T
import Graphics.RedViz.Drawable
import Graphics.RedViz.Descriptor
import Graphics.RedViz.Backend
import Graphics.RedViz.Camera
import Graphics.RedViz.LoadShaders
import Graphics.RedViz.GLUtil.JuicyTextures
import Graphics.RedViz.GLUtil                 (readTexture, texture2DWrap)
import Graphics.RedViz.Rendering (bindTexture, loadTex)

import RIO (throwString)

type DTime = Double

data Game = Game
  { tick     :: Integer
  , mpos     :: Point V2 CInt
  , quitGame :: Bool
  , drs      :: [Drawable]
  , hmap     :: [(UUID, GLuint)]
  , txs      :: [Texture]
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
  , drs      = []
  , hmap     = []
  , txs      = []
  }

initSettings :: GameSettings
initSettings = GameSettings
  {
    resX = 800
  , resY = 600
  }

updateGame :: MSF (MaybeT (ReaderT GameSettings (ReaderT Double (StateT Game IO)))) () Bool
updateGame = gameLoop `untilMaybe` gameQuit `catchMaybe` exit
  where
    gameLoop = arrM (\_ -> (lift . lift . lift) gameLoop')
    gameQuit = arrM (\_ -> (lift . lift . lift) gameQuit')

    gameQuit' :: StateT Game IO Bool
    gameQuit' = TMSF.get >>= \s -> return $ quitGame s

    gameLoop' :: StateT Game IO Bool
    gameLoop' = do
      -- TMSF.get >>= \s -> liftIO $ print $ tick s
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
                        inc' k (Game c m q d h t) =
                          Game
                          { tick     = c + k
                          , mpos     = m
                          , quitGame = q
                          , drs      = incDrw (fromIntegral(c + k)) <$> d
                          , hmap     = h
                          , txs      = t
                          }
                          where
                            incDrw tick' drw0@(Drawable _ unis0 _ _) = 
                              drw0 { uniforms = incUnis unis0 }
                              where
                                incUnis unis0 =
                                  unis0 { u_time = tick' }
                     
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
                                           mmove' pos (Game c m q d h t) =
                                             Game
                                             { tick     = c
                                             , mpos     = pos
                                             , quitGame = q
                                             , drs      = d
                                             , hmap     = h
                                             , txs      = t
                                             }
  
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

    let config = 
          OpenGLConfig { glColorPrecision     = V4 8 8 8 0
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

--type Drawable   = ([Int], [Vertex3 Double])
type Pos        = (Double, Double)  
data Shape      = Square Pos Double
                deriving Show


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
  (idx, vs) <- loadGltf
  initResources vs idx 0

toDescriptor' :: FilePath -> IO [Descriptor]
toDescriptor' file = do
  stuff <- loadGltf'
  mapM (\(vs, idx) -> initResources idx vs 0) $ concat stuff

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
        -- ShaderInfo VertexShader   (FileSource "shaders/checkerboard/src/shader.vert"),
        -- ShaderInfo FragmentShader (FileSource "shaders/checkerboard/src/shader.frag")]
        ShaderInfo VertexShader   (FileSource "shaders/test/shader.vert"),
        ShaderInfo FragmentShader (FileSource "shaders/test/shader.frag")]
    currentProgram $= Just program

    -- || Unload buffers
    bindVertexArrayObject         $= Nothing

    return $ Descriptor triangles (fromIntegral numIndices) program

bufferOffset :: Integral a => a -> Ptr b
bufferOffset = plusPtr nullPtr . fromIntegral
  
renderOutput :: Window -> (Game, Maybe Bool) -> IO Bool
renderOutput _ ( _,Nothing) = quit >> return True
renderOutput window (g,_) = do
  let
    timer    = 0.01 * (fromIntegral $ tick g)
    -- d' = descriptor $ head (drs g) :: Descriptor
    ds' = descriptor <$> drs g     :: [Descriptor]

  clearColor $= Color4 timer 0.0 0.0 1.0
  GL.clear [ColorBuffer, DepthBuffer]

  GL.pointSize $= 10.0
  GL.blend $= Enabled
  GL.depthMask $= Enabled
  depthFunc $= Just Less
  cullFace  $= Just Back

  bindUniforms g

  mapM_ (\(Descriptor triangles numIndices prg) -> do
            -- let
            --   (Descriptor triangles numIndices prg) = d'
            bindVertexArrayObject $= Just triangles
            drawElements GL.Triangles numIndices GL.UnsignedInt nullPtr
        ) ds'
  -- let
  --   (Descriptor triangles numIndices prg) = d'
  -- bindVertexArrayObject $= Just triangles
  
  -- drawElements GL.Triangles numIndices GL.UnsignedInt nullPtr

  glSwapWindow window >> return False

type MousePos = (Double, Double)

bindUniforms :: Game -> IO ()
bindUniforms g =
  do
    let
      txs' = txs g
      dr   = head (drs g)
      (Uniforms u_time' u_res' u_cam' u_cam_a' u_cam_f' u_xform' u_ypr' u_yprS' u_vel' u_accel') =
        uniforms dr
      d' = descriptor $ head (drs g) :: Descriptor
      --(Descriptor _ _ u_prog') = d'      
      u_mouse' = (0,0)
      hmap'     = hmap g

    u_prog' <- if True
      then
      loadShaders [
      ShaderInfo VertexShader   (FileSource "shaders/test/shader.vert"),
      ShaderInfo FragmentShader (FileSource "shaders/test/shader.frag")]
      else
      (\(Descriptor _ _ u_prog') -> return u_prog') d'

    currentProgram $= Just u_prog'

    let u_mouse0      = Vector2 (realToFrac $ fst u_mouse') (realToFrac $ snd u_mouse') :: Vector2 GLfloat
    location0         <- SV.get (uniformLocation u_prog' "u_mouse'")
    uniform location0 $= u_mouse0

    let resX          = fromIntegral $ fromEnum $ fst u_res' :: Double
        resY          = fromIntegral $ fromEnum $ snd u_res' :: Double
        u_res         = Vector2 (realToFrac resX) (realToFrac resY) :: Vector2 GLfloat

    location1         <- SV.get (uniformLocation u_prog' "u_resolution")
    uniform location1 $= u_res
    
    location2         <- SV.get (uniformLocation u_prog' "u_time")
    uniform location2 $= (double2Float u_time' :: GLfloat)

    let apt = u_cam_a' -- aperture
        foc = u_cam_f' -- focal length
        proj =
          LP.infinitePerspective
          (2.0 * atan ( apt/2.0 / foc )) -- FOV
          (resX/resY)                    -- Aspect
          0.01                           -- Near

    persp             <- GL.newMatrix RowMajor $ toList' proj   :: IO (GLmatrix GLfloat)
    location3         <- SV.get (uniformLocation u_prog' "persp")
    uniform location3 $= persp

    --print $ show u_cam'
    camera            <- GL.newMatrix RowMajor $ toList' u_cam' :: IO (GLmatrix GLfloat)
    location4         <- SV.get (uniformLocation u_prog' "camera")
    uniform location4 $= camera

    xform             <- GL.newMatrix RowMajor $ toList' (xform' u_xform' u_cam') :: IO (GLmatrix GLfloat)
    location5         <- SV.get (uniformLocation u_prog' "xform")
    uniform location5 $= xform

    xform1            <- GL.newMatrix RowMajor $ toList' u_xform' :: IO (GLmatrix GLfloat)
    location6         <- SV.get (uniformLocation u_prog' "xform1")
    uniform location6 $= xform1

    let sunP = GL.Vector3 299999999999.0 0.0 0.0 :: GL.Vector3 GLfloat
    location7 <- SV.get (uniformLocation u_prog' "sunP")
    uniform location7 $= sunP
    
    let ypr  =
          Vector3
          (double2Float $ u_ypr'^._1)
          (double2Float $ u_ypr'^._2)
          (double2Float $ u_ypr'^._3)
          :: Vector3 GLfloat
    location8        <- SV.get (uniformLocation u_prog' "ypr")
    uniform location8 $= ypr

    let yprS =
          Vector3
          (double2Float $ u_yprS'^._1)
          (double2Float $ u_yprS'^._2)
          (double2Float $ u_yprS'^._3)
          :: Vector3 GLfloat
    location9        <- SV.get (uniformLocation u_prog' "yprS")
    uniform location9 $= yprS


    let vel  =
          Vector3
          (double2Float $ u_vel'^._1)
          (double2Float $ u_vel'^._2)
          (double2Float $ u_vel'^._3)
          :: Vector3 GLfloat
    location10        <- SV.get (uniformLocation u_prog' "vel")
    uniform location10 $= vel

    let accel  =
          Vector3
          (double2Float $ u_accel'^._1)
          (double2Float $ u_accel'^._2)
          (double2Float $ u_accel'^._3)
          :: Vector3 GLfloat
    location11        <- SV.get (uniformLocation u_prog' "accel")
    uniform location11 $= accel

    -- || Set Transform Matrix
    let tr :: [GLfloat]
        tr =
          [ 1, 0, 0, 0
          , 0, 1, 0, 0
          , 0, 0, 1, 0
          , 0, 0, 0, 1 ]

    transform <- GL.newMatrix ColumnMajor tr :: IO (GLmatrix GLfloat)
    location12 <- SV.get (uniformLocation u_prog' "transform")
    uniform location12 $= transform

    -- | Allocate Textures
    texture Texture2D        $= Enabled
    mapM_ (allocateTextures u_prog' hmap') txs'

    -- | Unload buffers
    bindVertexArrayObject         $= Nothing
    bindBuffer ElementArrayBuffer $= Nothing
      where        
        toList' = fmap realToFrac.concat.(fmap DF.toList.DF.toList) :: V4 (V4 Double) -> [GLfloat]
        xform' u_xform' u_cam'= --- | = Object Position - Camera Position
          transpose $
          fromV3M44
          ( u_xform' ^._xyz )
          ( fromV3V4 (transpose u_xform' ^._w._xyz + transpose u_cam' ^._w._xyz) 1.0 ) :: M44 Double

allocateTextures :: Program -> [(UUID, GLuint)] -> Texture -> IO ()
allocateTextures program0 hmap tx =
  do
    location <- SV.get (uniformLocation program0 (T.name tx))
    uniform location $= TextureUnit txid
      where
        txid = fromMaybe 0 (lookup (uuid tx) hmap)

fromList :: [a] -> M44 a
fromList xs = V4
              (V4 (head xs ) (xs!!1 )(xs!!2 )(xs!!3))
              (V4 (xs!!4 ) (xs!!5 )(xs!!6 )(xs!!7))
              (V4 (xs!!8 ) (xs!!9 )(xs!!10)(xs!!11))
              (V4 (xs!!12) (xs!!13)(xs!!14)(xs!!15))

fromV3M44 :: V3 (V4 a) -> V4 a -> M44 a
fromV3M44 v3 = V4 (v3 ^. _x) (v3 ^. _y) (v3 ^. _z)

fromV3V4 :: V3 a -> a -> V4 a
fromV3V4 v3 = V4 (v3 ^. _x) (v3 ^. _y) (v3 ^. _z)

animate :: Window
         -> Game
         -> MSF (MaybeT (ReaderT GameSettings (ReaderT DTime (StateT Game IO)))) () Bool
         -> IO ()
animate window g sf = do
  reactimateB $ input >>> sfIO >>> output window
  quit
  where
    input    = arr (const (0.2, (initSettings, ())))                  :: MSF IO b (DTime, (GameSettings, ()))
    sfIO     = runStateS_ (runReaderS (runReaderS (runMaybeS sf))) g :: MSF IO   (DTime, (GameSettings, ())) (Game, Maybe Bool)
    output w = arrM (renderOutput w)                                  :: MSF IO   (Game, Maybe Bool) Bool

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
  d  <- toDescriptor "src/Model.gltf"
  ds <- toDescriptor' "src/Model.gltf" :: IO [Descriptor]
    
  let
    drw   = toDrawable "" 0.0 (resX', resY') defaultCam (identity :: M44 Double) defaultBackendOptions d
    --drws  = (\d -> toDrawable "" 0.0 (resX', resY') defaultCam (identity :: M44 Double) defaultBackendOptions d) <$> ds
    drws  = toDrawable "" 0.0 (resX', resY') defaultCam (identity :: M44 Double) defaultBackendOptions <$> ds :: [Drawable]
    --txs'  = [T.defaultTexture { path = "src/testgeometry_pighead_lowres.png"}]
    txs'  = [T.defaultTexture
             { path = "src/testgeometry_pighead_lowres.png"}
            ]
    uuids = fmap T.uuid txs'
    hmap' = DS.toList . DS.fromList $ zip uuids [0..]    
    initGame' =
      initGame { drs  = drws
               , hmap = hmap'
               , txs  = txs' }

  putStrLn "Binding Textures..."
  mapM_ (bindTexture hmap') txs'
      
  animate window initGame' updateGame 
  putStrLn "Exiting Game"

loadGltf :: IO ([GLenum],[GLfloat])
loadGltf = do
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

loadGltf' :: IO [[([GLenum],[GLfloat])]] -- > [[([GLenum],[GLfloat])]]
loadGltf' = do
  (root, meshPrimitives) <- loadMeshPrimitives False False "src/Model.gltf"
  let
    mgrs = V.toList <$> V.toList meshPrimitives :: [[Model_glTF.MeshPrimitive]]
    positions = (fmap.fmap) (\(maybeMatTuple, stuff) -> sPositions stuff) mgrs :: [[V.Vector Packed]]
    indices   = (fmap.fmap) (\(maybeMatTuple, stuff) -> sIndices   stuff) mgrs 
    idx       = (fmap.fmap.fmap) fromIntegral $ (fmap.fmap) V.toList indices 
    attrs     = (fmap.fmap) (\(maybeMatTuple, stuff) -> sAttrs     stuff) mgrs :: [[V.Vector VertexAttrs]]
    uvs       = (fmap.fmap.fmap) vaTexCoord $ (fmap.fmap) V.toList attrs 
    colors    = (fmap.fmap.fmap) vaRGBA     $ (fmap.fmap) V.toList attrs
    normals   = (fmap.fmap.fmap) vaNormal   $ (fmap.fmap) V.toList attrs

    ps = (fmap.fmap.fmap) (fromVec3' . unPacked) ((fmap.fmap) V.toList positions) :: [[[(Float,Float,Float)]]]
    cs = (fmap.fmap.fmap) fromVec4' colors :: [[[(Float,Float,Float,Float)]]]
    ts = (fmap.fmap.fmap) fromVec2' uvs
    d = (,,) <$$$.> ps <***.> cs <***.> ts
    verts = (fmap.fmap.concatMap) (\((x,y,z),(cr,cg,cb,ca),(u,v)) -> [x,y,z,cr,cg,cb,u,v]) d
  --return $ (\[x] [y] -> [zip x y]) idx verts
  return $ zipWith zip idx verts

(<$.>) :: (a -> b) -> [a] -> [b]
(<$.>) = fmap

(<$$$.>) :: (a -> b) -> [[[a]]] -> [[[b]]]
(<$$$.>) = fmap . fmap . fmap

(<*.>) :: [a -> b] -> [a] -> [b]
(<*.>) = zipWith ($)

(<***.>) :: [[[a -> b]]] -> [[[a]]] -> [[[b]]]
(<***.>) =  (zipWith . zipWith . zipWith) ($)

fromVec2' :: Vec2 -> (Float, Float)
fromVec2' xy = withVec2 (coerce xy) (,)
  
fromVec3' :: Vec3 -> (Float, Float, Float)
fromVec3' xyz = withVec3 (coerce xyz) (,,)

fromVec4' :: Vec4 -> (Float, Float, Float, Float)
fromVec4' xyzw = withVec4 (coerce xyzw) (,,,)

getVertices :: Gltf -> V.Vector (V3 Float)
getVertices gltf = V.concatMap getVertices' (gltf ^. _meshes)
  where getVertices' mesh = V.concatMap (^. _meshPrimitivePositions) (mesh ^. _meshPrimitives)

getIndices :: Gltf -> V.Vector Word16
getIndices gltf = V.concatMap getVertices' (gltf ^. _meshes)
  where getVertices' mesh = V.concatMap (^. _meshPrimitiveIndices) (mesh ^. _meshPrimitives)
