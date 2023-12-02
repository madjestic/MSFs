{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE DeriveGeneric #-}
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
import           Data.Text (Text, unpack) 
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
import           Text.GLTF.Loader as Gltf hiding (Texture, Material)
import           Codec.GlTF.Material as Gltf
import           Lens.Micro
import           Control.Lens.Combinators (view)
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
import Linear.Matrix
import Data.Maybe (fromMaybe)
import Data.Set as DS ( fromList, toList )
import GHC.Generics

import Load_glTF (loadMeshPrimitives)
import Model_glTF
import Projects.Test

--import Graphics.RedViz.Project as P
import Graphics.RedViz.Texture as T
import Graphics.RedViz.Descriptor
import Graphics.RedViz.Backend
import Graphics.RedViz.LoadShaders
import Graphics.RedViz.GLUtil.JuicyTextures
import Graphics.RedViz.GLUtil                 (readTexture, texture2DWrap)
import Graphics.RedViz.Rendering (bindTexture, bindTexture', loadTex)
import Graphics.RedViz.Material as R

import RIO (throwString)

type DTime = Double

data Controllable
  =  Controller
     { debug      :: (Int, Int)
     , transform  :: M44 Double
     , vel        :: V3 Double  -- velocity
     , ypr        :: V3 Double  -- yaw/pitch/camRoll
     , yprS       :: V3 Double  -- yaw/pitch/camRoll Sum
     }
  deriving Show

data Camera =
     Camera
     { name       :: String
     , apt        :: Double
     , foc        :: Double
     , controller :: Controllable
     , mouseS     :: V3 Double -- mouse    "sensitivity"
     , keyboardRS :: V3 Double -- keyboard "rotation sensitivity"
     , keyboardTS :: V3 Double -- keyboard "translation sensitivity"
     } deriving Show

defaultCam :: Camera
defaultCam =
  Camera
  {
    name       = "PlayerCamera"
  , apt        = 50.0
  , foc        = 100.0
  , controller = defaultCamController
  , mouseS     = -0.001
  , keyboardRS = 0.1
  , keyboardTS = 1.0
  }

defaultCamController :: Controllable
defaultCamController =
  ( Controller
    { debug = (0,0)
    , transform =  
      (V4
        (V4 1 0 0 0)  -- <- . . . x ...
        (V4 0 1 0 0)  -- <- . . . y ...
        (V4 0 0 1 10) -- <- . . . z-component of transform
        (V4 0 0 0 1))
    , vel  = (V3 0 0 0) -- velocity
    , ypr  = (V3 0 0 0) -- rotation
    , yprS = (V3 0 0 0) -- sum of rotations
    }
  )

data CoordSys =
    WorldSpace
  | ObjectSpace
  deriving Show

data Solver =
    Identity
  | Translate
    { space :: CoordSys
    , txyz  :: V3 Double -- offset
    , tvel  :: V3 Double -- velocity
    }
  | Rotate
    { space :: CoordSys
    , cxyz  :: V3 Double -- center of rotation
    , rord  :: RotationOrder
    , rxyz  :: V3 Double
    , avel  :: V3 Double -- angular velocity
    }
  deriving Show

data RotationOrder =
  XYZ

instance Show RotationOrder where
  show XYZ = "XYZ"
  show _   = error "RotationOrder undefined"

data PreObject
  =  PreObject
     { pname          :: String
     , ptype          :: String
     , pidx           :: Integer
     , uuid           :: UUID
     , modelIDXs      :: [Int]
     , presolvers     :: [String]
     , presolverAttrs :: [[Double]]
     , solvers        :: [Solver]
     , options        :: BackendOptions }
  |  PreFontObject
     { pname          :: String
     , ptype          :: String
     , pidx           :: Integer
     , uuid           :: UUID
     , modelIDXs      :: [Int]
     , presolvers     :: [String]
     , presolverAttrs :: [[Double]]
     , solvers        :: [Solver]
     , options        :: BackendOptions     
     } deriving Show

data Object
  =  Object
     { xform    :: M44 Double
     , drws     :: [Drawable]
     , slvrs    :: [Solver]
     } deriving Show

toObjects :: Project -> [(Texture, TextureObject)] -> [[(Descriptor, R.Material)]]-> IO [Object]
toObjects prj txTuples dms = mapM (toObject prj txTuples dms) (preObjects prj)

toFontObjects :: Project -> [(Texture, TextureObject)] -> [[(Descriptor, R.Material)]]-> IO [Object]
toFontObjects prj txTuples dms = mapM (toObject prj txTuples dms) (preFontObject prj)

testM44 :: M44 Double  
testM44 =
  (V4
    (V4 1 0 0 0.5) -- <- . . . x ...
    (V4 0 1 0 0)   -- <- . . . y ...
    (V4 0 0 1 0)   -- <- . . . z-component of transform
    (V4 0 0 0 1))
  
toObject :: Project -> [(Texture, TextureObject)] -> [[(Descriptor, R.Material)]]-> PreObject -> IO Object
toObject proj txTuples' dms' pobj = do
  let
    models' = case pobj of
      PreObject {}     -> (\idx -> models     proj!!idx) <$> modelIDXs pobj
      PreFontObject {} -> (\idx -> fontModels proj!!idx) <$> modelIDXs pobj
    dms     = (dms'!!) <$> modelIDXs pobj
  
  let
    txs          = concatMap (\(_,m) -> R.textures m) $ concat dms :: [Texture]
    txTuples     = filter (\(tx, txo) -> tx `elem` txs) txTuples' :: [(Texture, TextureObject)]
    (resX, resY) = (fromIntegral (resx proj), fromIntegral (resy proj))

    drs =
      toDrawable
      ""
      0.0
      (resX, resY)
      (camera initGame)
      --testM44
      (identity :: M44 Double) -- TODO: add result based on solvers composition
      defaultBackendOptions
      txTuples
      <$> concat dms
      :: [Drawable]

    obj =
      Object
      { xform  = identity :: M44 Double
      , drws   = drs
      , slvrs  = solvers pobj }

  return obj

data Drawable
  =  Drawable
     { descriptor :: Descriptor
     , material   :: R.Material
     , dtxs       :: [(Int, (Texture, TextureObject))]
     , doptions   :: BackendOptions
     , u_xform    :: M44 Double
     } deriving Show

data Uniforms
  =  Uniforms
     { u_time  :: Double
     , u_res   :: (CInt, CInt)
     , u_cam   :: M44 Double
     , u_cam_a :: Double
     , u_cam_f :: Double
     , u_cam_ypr   :: (Double, Double, Double)
     , u_cam_yprS  :: (Double, Double, Double)
     , u_cam_vel   :: (Double, Double, Double)
     , u_cam_accel :: (Double, Double, Double)
     } deriving Show

defaultUniforms :: Uniforms
defaultUniforms = 
  Uniforms
  { u_time  = 0.0
  , u_res   = (800,600)
  , u_cam   = identity :: M44 Double
  , u_cam_a = 50.0
  , u_cam_f = 100.0
  , u_cam_ypr   = (\(V3 x y z) -> (x,y,z)) $ ypr  defaultCamController
  , u_cam_yprS  = (\(V3 x y z) -> (x,y,z)) $ yprS defaultCamController
  , u_cam_vel   = (\(V3 x y z) -> (x,y,z)) $ vel  defaultCamController
  , u_cam_accel = (0,0,0) }

data Project
  =  Project
     {
       projname       :: String
     , resx           :: Int
     , resy           :: Int
     , camMode        :: String
     , models         :: [FilePath]
     , fontModels     :: [FilePath]
     , preObjects     :: [PreObject]
     , preFontObject :: [PreObject]
     , cameras        :: [Camera]
     } deriving Show

initProject :: Int -> Int -> Project
initProject resx' resy' =
  Project
  {  
    projname = "Test Project"
  , resx    = resx'
  , resy    = resy'
  , camMode = "AbsoluteLocation"
  , models  =
    [ "src/pighead.gltf"
    , "src/grid.gltf"
    ]
  , fontModels =
    [ "src/fnt_space.gltf"
    , "src/fnt_0.gltf"
    , "src/fnt_1.gltf"
    , "src/fnt_2.gltf"
    , "src/fnt_3.gltf"
    , "src/fnt_4.gltf"
    , "src/fnt_5.gltf"
    , "src/fnt_6.gltf"
    , "src/fnt_7.gltf"
    , "src/fnt_8.gltf"
    , "src/fnt_9.gltf"
    , "src/fnt_a.gltf"
    , "src/fnt_b.gltf"
    , "src/fnt_c.gltf"
    , "src/fnt_d.gltf"
    , "src/fnt_e.gltf"
    , "src/fnt_f.gltf"
    , "src/fnt_g.gltf"
    , "src/fnt_h.gltf"
    , "src/fnt_i.gltf"
    , "src/fnt_j.gltf"
    , "src/fnt_k.gltf"
    , "src/fnt_l.gltf"
    , "src/fnt_m.gltf"
    , "src/fnt_n.gltf"
    , "src/fnt_o.gltf"
    , "src/fnt_p.gltf"
    , "src/fnt_q.gltf"
    , "src/fnt_r.gltf"
    , "src/fnt_s.gltf"
    , "src/fnt_t.gltf"
    , "src/fnt_u.gltf"
    , "src/fnt_v.gltf"
    , "src/fnt_w.gltf"
    , "src/fnt_x.gltf"
    , "src/fnt_y.gltf"
    , "src/fnt_z.gltf"
    , "src/fnt_plus.gltf"
    , "src/fnt_minus.gltf"
    , "src/fnt_equal.gltf"
    , "src/fnt_gt.gltf"
    , "src/fnt_comma.gltf"
    , "src/fnt_dot.gltf"
    , "src/fnt_question.gltf"
    , "src/fnt_exclam.gltf"
    , "src/fnt_asterics.gltf"
    , "src/fnt_slash.gltf"
    , "src/fnt_semicolon.gltf"
    , "src/fnt_quote.gltf"
    , "src/fnt_A.gltf"
    , "src/fnt_B.gltf"
    , "src/fnt_C.gltf"
    , "src/fnt_D.gltf"
    , "src/fnt_E.gltf"
    , "src/fnt_F.gltf"
    , "src/fnt_G.gltf"
    , "src/fnt_H.gltf"
    , "src/fnt_I.gltf"
    , "src/fnt_J.gltf"
    , "src/fnt_K.gltf"
    , "src/fnt_L.gltf"
    , "src/fnt_M.gltf"
    , "src/fnt_N.gltf"
    , "src/fnt_O.gltf"
    , "src/fnt_P.gltf"
    , "src/fnt_Q.gltf"
    , "src/fnt_R.gltf"
    , "src/fnt_S.gltf"
    , "src/fnt_T.gltf"
    , "src/fnt_U.gltf"
    , "src/fnt_V.gltf"
    , "src/fnt_W.gltf"
    , "src/fnt_X.gltf"
    , "src/fnt_Y.gltf"
    , "src/fnt_Z.gltf"
    , "src/fnt_crosshair.gltf"
    ]
  , preObjects = 
    [ PreObject
      {
        pname          = "test_object"
      , ptype          = "default"
      , pidx           = 0
      , uuid           = nil
      , modelIDXs      = [0,1]
      , presolvers     = []
      , presolverAttrs = []
      , solvers        =
        [ Identity
        , Translate
          { space = WorldSpace
          , txyz  = V3 1.1 0 0
          , tvel  = V3 0.0 0 0 }
        , Rotate
          { space = ObjectSpace
          , cxyz  = V3 1.1 0 0
          , rord  = XYZ
          , rxyz  = V3 0 0 (0.0)
          , avel  = V3 0 0 0.01 }
        -- , Translate
        --  { space = WorldSpace
        --  , txyz  = V3 1.1 0 0
        --  , tvel  = V3 0.0 0 0 }
        --  , Identity
         ]
        , options        = defaultBackendOptions
      }
    ]
  , preFontObject =
    [ PreFontObject
      {
        pname          = "fonts"
      , ptype          = "default"
      , pidx           = 0
      , uuid           = nil
      , modelIDXs      = [0..75]
      , presolvers     = []
      , presolverAttrs = []
      , solvers        = [ Identity
                         ]
      , options        = defaultBackendOptions
      }
    ]
  , cameras    = [ defaultCam ]
  }

data Alignment =
   TL |TC |TR
  |CL |CC |CR
  |BL |BC |BR
  deriving (Generic, Show)


data Format -- move to Format.hs?
  =  Format
     { alignment :: Alignment
     , xres      :: Int
     , yres      :: Int
     , xoffset   :: Double
     , yoffset   :: Double
     , zoffset   :: Double
     , soffset   :: Double -- scale Offset
     , ssize     :: Double -- scale Size
     } deriving (Generic, Show)

data Widget
  =  Empty
  |  TextField
     { active   :: Bool
     , text     :: [String]
     , fonts    :: [Object]
     , format   :: Format
     , optionsW :: BackendOptions
     }
  |  Cursor
     { active   :: Bool
     , fonts    :: [Object]     
     , cpos     :: Point V2 CInt
     , optionsW :: BackendOptions
     } deriving (Generic, Show)

data Game = Game
  { tick     :: Integer
  , mpos     :: Point V2 CInt
  , quitGame :: Bool
  , camera   :: Camera
  , uniforms :: Uniforms
  , objs     :: [Object]
  , gui      :: [Widget]
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
  , camera   = defaultCam
  , uniforms = defaultUniforms
  , objs     = []
  , gui      = []
  }

initSettings :: GameSettings
initSettings = GameSettings
  { resX = 1280
  , resY = 720 }

type Time = Double
type Res  = (CInt, CInt)

unzipWith :: Eq a => [a] -> [(a,b)] -> [(a,b)]
unzipWith xs xys = xys'
  where
    xys' = filter (\xy -> fst xy `elem` xs) xys

toDrawable
  :: String
  -> Time
  -> Res
  -> Camera
  -> M44 Double
  -> BackendOptions
  -> [(Texture, TextureObject)]
  -> (Descriptor, R.Material)
  -> Drawable
toDrawable name' time' res' cam xform' opts txos (d, mat') = dr
  where
    apt'   = apt cam
    foc'   = foc cam
    xformC =  transform (controller cam) :: M44 Double
    txs'   = R.textures mat'
    txos'  = zip [0..] $ unzipWith txs' txos :: [(Int, (Texture, TextureObject))] 
    dr =
      Drawable
      { 
        u_xform    = xform'
      , descriptor = d
      , material   = mat'
      , dtxs       = txos'
      , doptions   = opts
      }

updateGame :: MSF (MaybeT (ReaderT GameSettings (ReaderT Double (StateT Game IO)))) () Bool
updateGame = gameLoop `untilMaybe` gameQuit `catchMaybe` exit
  where
    gameLoop = arrM (\_ -> (lift . lift) gameLoopDelay)
    gameQuit = arrM (\_ -> (lift . lift . lift) gameQuit')

    gameQuit' :: StateT Game IO Bool
    gameQuit' = TMSF.get >>= \s -> return $ quitGame s

    gameLoopDelay :: ReaderT Double (StateT Game IO) Bool
    gameLoopDelay = do
      TMSF.ask >>= \r ->  liftIO $ delay $ fromIntegral(double2Int $ r * 10)
      lift gameLoop'

    gameLoop' :: StateT Game IO Bool
    gameLoop' = do

      updateObjects
      --updateGUI
      handleEvents
        where
          updateObjects :: StateT Game IO ()
          updateObjects = do
            modify solveObjs
            return ()
              where                  
                solveObjs :: Game -> Game
                solveObjs g0 =
                  g0 { objs = (\obj -> foldr1 (!@!) $ solve (obj {slvrs = []}) <$> slvrs obj ) <$> objs g0 }
                  where
                    (!@!) :: Object -> Object -> Object
                    (!@!) obj0 obj1 =
                      obj0
                      { xform = xform obj1 !*! xform obj0
                      , slvrs = slvrs obj0 ++ slvrs obj1
                      }
                     
                    updateSolver :: Solver -> Solver
                    updateSolver slv =
                      case slv of
                        Identity                 -> slv
                        Translate _ pos vel      -> slv { txyz = pos + vel }
                        Rotate _ _ _ rxyz' avel' -> slv { rxyz = rxyz' + avel' }

                    solve :: Object -> Solver -> Object
                    solve obj slv =
                      case slv of
                        Identity -> obj { xform = identity }
                        Translate cs pos vel ->
                          case cs of
                            WorldSpace  ->
                              obj
                              { xform = identity & translation .~ pos
                              , slvrs = [updateSolver slv]
                              }
                            ObjectSpace -> undefined

                        Rotate cs pos rord rxyz avel ->
                          obj
                          { xform = transform identity
                          , slvrs = [updateSolver slv]
                          }
                          where
                            transform :: M44 Double -> M44 Double
                            transform mtx0 = mtx
                              where
                                mtx =
                                  mkTransformationMat
                                  rot
                                  tr
                                  where
                                    rot    = 
                                      identity !*!
                                      case rord of
                                        XYZ ->
                                              fromQuaternion (axisAngle (mtx0^.(_m33._x)) (rxyz^._x)) -- pitch
                                          !*! fromQuaternion (axisAngle (mtx0^.(_m33._y)) (rxyz^._y)) -- yaw
                                          !*! fromQuaternion (axisAngle (mtx0^.(_m33._z)) (rxyz^._z)) -- roll
                                    tr     = (identity::M44 Double)^.translation

                        _ -> error $ "solver " ++ show slv ++ " is not found"

          --updateGUI :: StateT Game IO ()
          --updateGUI = undefined

          handleEvents :: StateT Game IO Bool
          handleEvents = do
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

                updateKeyboard :: (Monad m) => [(Scancode, m ())] -> [Event] -> m ()
                updateKeyboard emap = mapM_ (processEvent emap)
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
                
                mapKeyEvents :: [(Scancode, StateT Game IO ())]
                mapKeyEvents =
                  [ (ScancodeW, inc   10)
                  , (ScancodeS, inc (-10))
                  , (ScancodeEscape, quit True)
                  , (ScancodeQ, camRoll   1)
                  , (ScancodeE, camRoll (-1))
                  ]
                  where
                    camRoll :: Integer -> StateT Game IO ()
                    camRoll n = modify $ camRoll' n
                      where
                        camRoll' :: Integer -> Game -> Game
                        camRoll' k g0 = g0 { camera = updateCam n cam0 }
                          where
                            cam0            = camera g0
                            updateCam :: Integer -> Camera -> Camera
                            updateCam n cam =
                              cam { controller = updateController n (controller cam)}
                              where
                                updateController :: Integer -> Controllable -> Controllable
                                updateController pos ctrl@(Controller _ mtx0 _ ypr0 _) =
                                  ctrl
                                  { transform = 
                                      mkTransformationMat
                                      rot
                                      tr
                                  }
                                  where
                                    tr = view translation mtx0
                                    rot = 
                                      (mtx0^._m33)
                                      !*! fromQuaternion (axisAngle (mtx0^.(_m33._z)) (keyboardRS cam^._x * (fromIntegral n))) -- yaw
                          
                    inc :: Integer -> StateT Game IO ()
                    inc n = modify $ inc' n
                      where
                        inc' :: Integer -> Game -> Game
                        inc' k g0 = g0 { uniforms = incUnis (fromIntegral(tick g0 + k)) (uniforms g0) }
                          where
                            incUnis :: Integer -> Uniforms -> Uniforms
                            incUnis tick' unis0 = 
                              unis0 { u_time = fromInteger tick' }
                     
                    quit :: Bool -> StateT Game IO ()
                    quit b = modify $ quit' b
                      where
                        quit' :: Bool -> Game -> Game
                        quit' b gameLoopDelay = gameLoopDelay { quitGame = b }

                updateMouse  :: [Event] -> StateT Game IO ()
                updateMouse = mapM_ processEvent 
                  where
                    processEvent :: Event -> StateT Game IO ()
                    processEvent e =
                      let mk = case eventPayload e of
                            MouseMotionEvent mouseEvent -> Just (mouseMotionEventRelMotion mouseEvent)
                            _ -> Nothing
                      in case mk of
                        Nothing   -> return ()
                        Just vpos ->
                          mmove (unsafeCoerce vpos)
                          where
                            mmove :: Point V2 CInt -> StateT Game IO ()
                            mmove pos = do
                              modify $ mmove' pos
                              where
                                mmove' :: Point V2 CInt -> Game -> Game
                                mmove' pos g0 = g0 { camera = updateCam pos (camera g0) }
                                  where
                                    updateCam pos cam =
                                      cam { controller = updateController pos (controller cam)}
                                      where
                                        updateController :: Point V2 CInt -> Controllable -> Controllable
                                        updateController pos ctrl@(Controller _ mtx0 _ ypr0 _) =
                                          ctrl
                                          { transform = 
                                              mkTransformationMat
                                              rot
                                              tr
                                          }
                                          where
                                            tr = view translation mtx0
                                            rot = 
                                              (mtx0^._m33)
                                              !*! fromQuaternion (axisAngle (mtx0^.(_m33._x)) (mouseS cam^._x * (fromIntegral $ pos^._y))) -- pitch
                                              !*! fromQuaternion (axisAngle (mtx0^.(_m33._y)) (mouseS cam^._x * (fromIntegral $ pos^._x))) -- yaw

         
  
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

toDescriptorMat :: FilePath -> IO [(Descriptor, R.Material)]
toDescriptorMat file = do
  (stuff, mats) <- loadGltf file -- "src/pighead.gltf"
  mats' <- mapM fromGltfMaterial mats
  print mats'
  ds    <- mapM (\((vs, idx), mat) -> initResources idx vs mat 0) $ zip (concat stuff) mats'
  return $ zip ds mats'
    where
      fromGltfMaterial :: Gltf.Material -> IO R.Material
      fromGltfMaterial mat =
        R.read 
        $ case Gltf.name mat of
            Nothing -> "./mat/checkerboard/checkerboard"
            Just s  -> "./mat/" ++ unpack s ++ "/" ++ unpack s

fromVertex3 :: Vertex3 Double -> [GLfloat]
fromVertex3 (Vertex3 x y z) = [double2Float x, double2Float y, double2Float z]

initResources :: [GLfloat] -> [GLenum] -> R.Material -> Double -> IO Descriptor
initResources vs idx mat z0 =  
  do
    -- print $ mat
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
    -- print $ "mat : " ++ show mat
    program <- loadShaders [
        ShaderInfo VertexShader   (FileSource $ vertShader mat),
        ShaderInfo FragmentShader (FileSource $ fragShader mat)]
    currentProgram $= Just program

    -- || Unload buffers
    bindVertexArrayObject         $= Nothing

    return $ Descriptor triangles (fromIntegral numIndices) program

bufferOffset :: Integral a => a -> Ptr b
bufferOffset = plusPtr nullPtr . fromIntegral
  
renderOutput :: Window -> GameSettings -> (Game, Maybe Bool) -> IO Bool
renderOutput _ _ ( _,Nothing) = quit >> return True
renderOutput window gs (g,_) = do
  let
    timer = 0.01 * (fromIntegral $ tick g)
    --ds'   = descriptor <$> drs g :: [Descriptor]

  clearColor $= Color4 0.0 0.0 0.0 1.0
  GL.clear [ColorBuffer, DepthBuffer]

  GL.pointSize $= 10.0
  GL.blend $= Enabled
  GL.depthMask $= Enabled
  depthFunc $= Just Less
  cullFace  $= Just Back

  mapM_ (renderObject (camera g) (uniforms g)) (objs g)
  mapM_ (renderWidget (camera g) (uniforms g)) (gui  g)

  glSwapWindow window >> return False

renderWidget :: Camera -> Uniforms -> Widget -> IO ()
renderWidget cam unis' wgt = case wgt of
  Empty                   -> do return ()
  Cursor  False _ _ _     -> do return ()
  Cursor  _ fnts cpos opts ->
    (\dr -> do
        bindUniforms cam unis' (formatDrw (format wgt) dr) 
        let (Descriptor triangles numIndices _) = descriptor dr
        bindVertexArrayObject $= Just triangles
        drawElements GL.Triangles numIndices GL.UnsignedInt nullPtr
        ) (wdrs!!75) -- cursor font index is 75
  TextField False _ _ _ _   -> do return ()
  TextField _ s fnts fmt opts -> -- TODO: add String rendering
    mapM_
    (\dr -> do
        bindUniforms cam unis' dr 
        let (Descriptor triangles numIndices _) = descriptor dr
        bindVertexArrayObject $= Just triangles
        drawElements GL.Triangles numIndices GL.UnsignedInt nullPtr
        ) $ formatText fmt wdrs s (0,0)
  _ -> error "Unknown Widget type"
  where
    wdrs = concatMap drws (fonts wgt)

type CursorPos = (Integer, Integer)

formatText :: Format -> [Drawable] -> [String] -> CursorPos -> [Drawable]
formatText fmt drws [] _  = []
formatText fmt drws [s] (x,y) =
  formatString fmt drws s (x,y)
formatText fmt drws (s:ss) (x,y) =
  formatText fmt drws [s] (x,y) ++ formatText fmt drws ss (x,y+1)

formatString :: Format -> [Drawable] -> String -> CursorPos -> [Drawable]
formatString fmt drws []     (x,y) = []
formatString fmt drws [c]    (x,y) = [formatChar fmt drws c (x,y)]
formatString fmt drws (c:cs) (x,y) =  formatChar fmt drws c (x,y) : formatString fmt drws cs (x+1,y)

formatChar :: Format -> [Drawable] -> Char -> CursorPos -> Drawable
formatChar fmt drws chr cpos =
  case chr of
    ' ' -> offsetDrw cpos (drws!!0)
    '0' -> offsetDrw cpos (drws!!1)
    '1' -> offsetDrw cpos (drws!!2)
    '2' -> offsetDrw cpos (drws!!3)
    '3' -> offsetDrw cpos (drws!!4)
    '4' -> offsetDrw cpos (drws!!5)
    '5' -> offsetDrw cpos (drws!!6)
    '6' -> offsetDrw cpos (drws!!7)
    '7' -> offsetDrw cpos (drws!!8)
    '8' -> offsetDrw cpos (drws!!9)
    '9' -> offsetDrw cpos (drws!!10)
    'a' -> offsetDrw cpos (drws!!11)
    'b' -> offsetDrw cpos (drws!!12)
    'c' -> offsetDrw cpos (drws!!13)
    'd' -> offsetDrw cpos (drws!!14)
    'e' -> offsetDrw cpos (drws!!15)
    'f' -> offsetDrw cpos (drws!!16)
    'g' -> offsetDrw cpos (drws!!17)
    'h' -> offsetDrw cpos (drws!!18)
    'i' -> offsetDrw cpos (drws!!19)
    'j' -> offsetDrw cpos (drws!!20)
    'k' -> offsetDrw cpos (drws!!21)
    'l' -> offsetDrw cpos (drws!!22)
    'm' -> offsetDrw cpos (drws!!23)
    'n' -> offsetDrw cpos (drws!!24)
    'o' -> offsetDrw cpos (drws!!25)
    'p' -> offsetDrw cpos (drws!!26)
    'q' -> offsetDrw cpos (drws!!27)
    'r' -> offsetDrw cpos (drws!!28)
    's' -> offsetDrw cpos (drws!!29)
    't' -> offsetDrw cpos (drws!!30)
    'u' -> offsetDrw cpos (drws!!31)
    'v' -> offsetDrw cpos (drws!!32)
    'w' -> offsetDrw cpos (drws!!33)
    'x' -> offsetDrw cpos (drws!!34)
    'y' -> offsetDrw cpos (drws!!35)
    'z' -> offsetDrw cpos (drws!!36)
    '+' -> offsetDrw cpos (drws!!37)
    '-' -> offsetDrw cpos (drws!!38)
    '=' -> offsetDrw cpos (drws!!39)
    '>' -> offsetDrw cpos (drws!!40)
    ',' -> offsetDrw cpos (drws!!41)
    '.' -> offsetDrw cpos (drws!!42)
    '?' -> offsetDrw cpos (drws!!43)
    '!' -> offsetDrw cpos (drws!!44)
    '*' -> offsetDrw cpos (drws!!45)
    '/' -> offsetDrw cpos (drws!!46)
    ';' -> offsetDrw cpos (drws!!47)
    '\''-> offsetDrw cpos (drws!!48)
    'A' -> offsetDrw cpos (drws!!49)
    'B' -> offsetDrw cpos (drws!!50)
    'C' -> offsetDrw cpos (drws!!51)
    'D' -> offsetDrw cpos (drws!!52)
    'E' -> offsetDrw cpos (drws!!53)
    'F' -> offsetDrw cpos (drws!!54)
    'G' -> offsetDrw cpos (drws!!55)
    'H' -> offsetDrw cpos (drws!!56)
    'I' -> offsetDrw cpos (drws!!57)
    'J' -> offsetDrw cpos (drws!!58)
    'K' -> offsetDrw cpos (drws!!59)
    'L' -> offsetDrw cpos (drws!!60)
    'M' -> offsetDrw cpos (drws!!61)
    'N' -> offsetDrw cpos (drws!!62)
    'O' -> offsetDrw cpos (drws!!63)
    'P' -> offsetDrw cpos (drws!!64)
    'Q' -> offsetDrw cpos (drws!!65)
    'R' -> offsetDrw cpos (drws!!66)
    'S' -> offsetDrw cpos (drws!!67)
    'T' -> offsetDrw cpos (drws!!68)
    'U' -> offsetDrw cpos (drws!!69)
    'V' -> offsetDrw cpos (drws!!70)
    'W' -> offsetDrw cpos (drws!!71)
    'X' -> offsetDrw cpos (drws!!72)
    'Y' -> offsetDrw cpos (drws!!73)
    'Z' -> offsetDrw cpos (drws!!74)
    _   -> head drws

  
offsetDrw :: CursorPos -> Drawable -> Drawable
offsetDrw cpos drw =
  drw { u_xform = mkTransformationMat rot tr }
  where
    sh  = 0.1
    sv  = -0.15
    rot = identity :: M33 Double
    tr  =
      (identity::M44 Double)^.translation
      +
      V3 (fromIntegral $ fst cpos) (fromIntegral $ snd cpos) 0.0
      *
      V3 sh sv 0.0
      

formatDrw :: Format -> Drawable -> Drawable
formatDrw fmt dr = dr

  
renderObject :: Camera -> Uniforms -> Object -> IO ()
renderObject cam unis' obj = do
  mapM_ (\dr -> do
            bindUniforms cam unis' dr {u_xform = (xform obj)} 
            let (Descriptor triangles numIndices _) = descriptor dr
            bindVertexArrayObject $= Just triangles
            drawElements GL.Triangles numIndices GL.UnsignedInt nullPtr
        ) (drws obj)
  
  
bindUniforms :: Camera -> Uniforms -> Drawable -> IO ()  
bindUniforms cam' unis' dr =  
  do
    let
      u_xform'  = u_xform  dr
      d'        = descriptor dr :: Descriptor
      u_cam'    = (transform.controller) cam'
      u_mouse'  = (0,0)
      (Uniforms u_time' u_res' _ u_cam_a' u_cam_f' u_ypr' u_yprS' u_vel' u_accel') = unis'
      (Descriptor _ _ u_prog') = d'

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
          (2.0 * atan ( apt/foc/2.0 )) -- FOV
          (resX/resY)                  -- Aspect
          0.01                         -- Near

    persp             <- GL.newMatrix RowMajor $ toList' proj   :: IO (GLmatrix GLfloat)
    location3         <- SV.get (uniformLocation u_prog' "persp")
    uniform location3 $= persp

    camera            <- GL.newMatrix RowMajor $ toList' u_cam' :: IO (GLmatrix GLfloat)
    location4         <- SV.get (uniformLocation u_prog' "camera")
    uniform location4 $= camera

    xform             <- GL.newMatrix RowMajor $ toList' (xformComp u_xform' u_cam') :: IO (GLmatrix GLfloat)
    --xform             <- GL.newMatrix RowMajor $ toList' (inv44 u_cam' !*! u_xform') :: IO (GLmatrix GLfloat)
    location5         <- SV.get (uniformLocation u_prog' "xform")
    uniform location5 $= xform

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
    mapM_ (allocateTextures u_prog') (dtxs dr) -- TODO: this is ignored, should bind an appropriate texture

    -- | Unload buffers
    bindVertexArrayObject         $= Nothing
    bindBuffer ElementArrayBuffer $= Nothing
      where        
        toList' = fmap realToFrac.concat.(fmap DF.toList.DF.toList) :: V4 (V4 Double) -> [GLfloat]
        -- | Compensate world space xform with camera position
        -- = Object Position - Camera Position
        xformComp :: M44 Double -> M44 Double -> M44 Double
        xformComp u_xform' u_cam'=
          (inv44 (identity & translation .~ u_cam'^.translation)) !*! u_xform'
          -- transpose $
          -- fromV3M44
          -- ( u_xform' ^._xyz )
          -- ( fromV3V4 (transpose u_xform' ^._w._xyz - transpose u_cam' ^._w._xyz) 1.0 ) :: M44 Double
          
allocateTextures :: Program -> (Int, (Texture, TextureObject)) -> IO ()
allocateTextures program0 (txid, (tx, txo)) =
  do
    activeTexture $= TextureUnit (fromIntegral txid)
    textureBinding Texture2D $= Just txo
    return ()

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
        -> DTime
        -> GameSettings
        -> Game
        -> MSF (MaybeT (ReaderT GameSettings (ReaderT DTime (StateT Game IO)))) () Bool
        -> IO ()
animate window dt gs g sf = do
  reactimateB $ input >>> sfIO >>> output window
  quit
  where
    input    = arr (const (dt, (gs, ())))                            :: MSF IO b (DTime, (GameSettings, ()))
    sfIO     = runStateS_ (runReaderS (runReaderS (runMaybeS sf))) g :: MSF IO   (DTime, (GameSettings, ())) (Game, Maybe Bool)
    output w = arrM (renderOutput w gs)                              :: MSF IO   (Game, Maybe Bool) Bool

main :: IO ()
main = do
  let
    (resX', resY') =
      (\opts ->
          ( unsafeCoerce $ fromIntegral $ resX opts
          , unsafeCoerce $ fromIntegral $ resY opts))
      initSettings
    initProject'= Main.initProject resX' resY'
    models'     = models     initProject' :: [FilePath]
    fonts'      = fontModels initProject' :: [FilePath]
  print $ "fonts' length : " ++ show (length fonts')
  
  -- TODO: if UUIDs are needed, generate like so:
  -- (const nextRandom) ()
  -- 10514e78-fa96-444a-8c3d-0a8445e771ad

  initializeAll
  window <- openWindow "Mandelbrot + SDL2/OpenGL" (resX', resY')

  _ <- setMouseLocationMode RelativeLocation
  _ <- warpMouse (WarpInWindow window) (P (V2 (resX'`div`2) (resY'`div`2)))
  _ <- cursorVisible $= True
  
  putStrLn "Compiling Materials"
  dms  <- mapM toDescriptorMat models' :: IO [[(Descriptor, R.Material)]]
  fdms <- mapM toDescriptorMat fonts'  :: IO [[(Descriptor, R.Material)]] --
  print $ "fdms length : " ++ show (length fdms)
    
  let -- this basically collects all the materials, reads textures from them and uniquely binds
    txs   = concatMap (\(_,m) -> R.textures m) $ concat dms
    uuids = fmap T.uuid txs
    txord = DS.toList . DS.fromList $ zip uuids [0..] -- this guarantees unique texture (uuid) bindings

    ftxs   = concatMap (\(_,m) -> R.textures m) $ concat fdms
    fuuids = fmap T.uuid ftxs
    ftxord = DS.toList . DS.fromList $ zip fuuids [0..] -- this guarantees unique texture (uuid) bindings
        
  putStrLn "Binding Textures..."
  txTuples  <- mapM (bindTexture'  txord) txs  :: IO [(Texture, TextureObject)]
  ftxTuples <- mapM (bindTexture' ftxord) ftxs :: IO [(Texture, TextureObject)]
  print $ "ftxTuples length : " ++ show (length ftxTuples)
  objs'    <- toObjects     initProject' txTuples  dms
  fobjs'   <- toFontObjects initProject' ftxTuples fdms
  print $ "fobjs' length : " ++ show (length fobjs')

  animate
    window
    (0.1 :: Double) -- time increment
    initSettings
    initGame
      { 
        objs      = objs'
      , uniforms =
          defaultUniforms
          { u_res   = (resX', resY')
          , u_cam_a = apt defaultCam
          , u_cam_f = foc defaultCam
          }
      , gui =
        [ Cursor
          { active = True
          , fonts  = fobjs'
          , cpos   = P (V2 0 0)
          , optionsW = defaultBackendOptions
          }
        , TextField
          { active = True
          , text   =
              [" SUKANAH!"
              ,"mnogobukav"
              ," ebobo?.."]
          , fonts  = fobjs'
          , format = Format
            {
              alignment = CC
            , xres      = resX'
            , yres      = resY'
            , xoffset   = 0.0
            , yoffset   = 0.0
            , zoffset   = 0.0
            , soffset   = 0.0
            , ssize     = 0.0
            }
          , optionsW = defaultBackendOptions
          }
        ]
      }
    updateGame
  
  putStrLn "Exiting Game"

defaultGltfMat :: Gltf.Material
defaultGltfMat = Gltf.Material
  { emissiveFactor = (0,0,0)
  , alphaMode      = MaterialAlphaMode {unMaterialAlphaMode = "OPAQUE"}
  , alphaCutoff    = 0.5
  , doubleSided    = False
  , pbrMetallicRoughness = Nothing
  , normalTexture        = Nothing
  , occlusionTexture     = Nothing
  , emissiveTexture      = Nothing
  , name                 = Just "test"
  , extensions           = Nothing
  , extras               = Nothing
  } 

loadGltf :: FilePath -> IO ([[([GLenum],[GLfloat])]], [Gltf.Material])
loadGltf fp = do
  (root, meshPrimitives) <- loadMeshPrimitives False False fp
  let
    mgrs = V.toList <$> V.toList meshPrimitives :: [[Model_glTF.MeshPrimitive]]
    positions = (fmap.fmap) (\(_, stuff) -> sPositions stuff) mgrs :: [[V.Vector Packed]]
    indices   = (fmap.fmap) (\(_, stuff) -> sIndices   stuff) mgrs 
    idx       = (fmap.fmap.fmap) fromIntegral $ (fmap.fmap) V.toList indices 
    attrs     = (fmap.fmap) (\(_, stuff) -> sAttrs     stuff) mgrs :: [[V.Vector VertexAttrs]]
    uvs       = (fmap.fmap.fmap) vaTexCoord $ (fmap.fmap) V.toList attrs 
    colors    = (fmap.fmap.fmap) vaRGBA     $ (fmap.fmap) V.toList attrs
    normals   = (fmap.fmap.fmap) vaNormal   $ (fmap.fmap) V.toList attrs
    matTuples = (fmap.fmap) (\(maybeMatTuple, _) -> fromMaybe (0, defaultGltfMat) maybeMatTuple) mgrs :: [[(Int, Gltf.Material)]]
    mats      = (fmap.fmap) snd matTuples :: [[Gltf.Material]]

    ps = (fmap.fmap.fmap) (fromVec3' . unPacked) ((fmap.fmap) V.toList positions) :: [[[(Float,Float,Float)]]]
    cs = (fmap.fmap.fmap) fromVec4' colors :: [[[(Float,Float,Float,Float)]]]
    ts = (fmap.fmap.fmap) fromVec2' uvs
    d = (,,) <$$$.> ps <***.> cs <***.> ts
    verts = (fmap.fmap.concatMap) (\((x,y,z),(cr,cg,cb,ca),(u,v)) -> [x,y,z,cr,cg,cb,u,v]) d
  return $ (zipWith zip idx verts, concat mats)

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
