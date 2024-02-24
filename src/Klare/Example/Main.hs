module Klare.Example.Main where

import Control.Concurrent.STM.TQueue
import Control.Exception (throwIO)
import Control.Exception.Base (Exception)
import Control.Monad
import Data.Bifunctor
import Data.Function
import Data.Monoid (Sum (..))
import Foreign (Storable (..))
import Foreign.Marshal.Array (withArray)
import Foreign.Ptr (Ptr, nullPtr, plusPtr)
import GHC.Exception (prettyCallStack)
import GHC.Float (double2Float)
import GHC.Stack (HasCallStack, callStack)
import Graphics.GLUtil qualified as GLUtil
import Graphics.Rendering.OpenGL (($=))
import Graphics.Rendering.OpenGL qualified as GL
import Graphics.UI.GLFW qualified as GLFW
import Klare.Data.Buffer
import Klare.Error.GL
import Klare.Events (handleEvents, withEventChann)
import Klare.Events.Type
import Klare.Image.Texture (readTexInfoFlipped)
import Klare.Info.Print
import Shaders.Load
import System.Exit (exitSuccess)
import Klare.Data.Texture (registerTextures)
import Control.Monad.Reader (ReaderT, asks, runReaderT)
import Control.Monad.State (StateT, runStateT, put, MonadState (..), modify, gets)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Ratio
import Linear.Matrix
import Data.Maybe
import Linear.Quaternion
import Klare.Math.Linear
import Linear
import Klare.Example.Geometry
import Data.Functor
import Text.Pretty.Simple (pPrint)
import Data.Ord

bufferOffset :: (Integral a) => a -> Ptr b
bufferOffset = plusPtr nullPtr . fromIntegral

data KlareEnv =
  KlareEnv
    { events :: !(TQueue Event)
    , window :: !GLFW.Window
    }

data KlareState =
  KlareState
    { winHeight :: !Int
    , winWidth :: !Int
    , cameraPos :: V3 GL.GLfloat
    , cameraFront :: V3 GL.GLfloat
    , cameraUp :: V3 GL.GLfloat
    , yaw :: GL.GLfloat
    , pitch :: GL.GLfloat
    , lastX :: Maybe Float
    , lastY :: Maybe Float
    , cursorInWindow :: Bool
    , mouseButtonPressed :: Bool
    , deltaTime :: Float
    , lastFrame :: Float
    }

type KlareM a = ReaderT KlareEnv (StateT KlareState IO) a

type VBO = (GL.Vertex3 GL.GLfloat, GL.Color3 GL.GLfloat, GL.TexCoord2 GL.GLfloat)

vtxCoord :: Bool -> GL.GLfloat
vtxCoord b = if b then 0.5 else -0.5

toVtx :: Vertices3D GL.GLfloat -> GL.Vertex3 GL.GLfloat
toVtx Vertices3D{..} = GL.Vertex3 x y z

texCoord :: Bool -> GL.GLfloat
texCoord b = if b then 1.0 else 0.0

texCoords :: Bool -> Bool -> GL.TexCoord2 GL.GLfloat
texCoords b b' = GL.TexCoord2 (texCoord b) (texCoord b')

texCoordsToColor :: Bool -> Bool -> GL.Color3 GL.GLfloat
texCoordsToColor x y = case (x, y) of
  (False, False) -> GL.Color3 1.0 0.0 0.0
  (True, False) -> GL.Color3 0.0 1.0 0.0
  (False, True) -> GL.Color3 0.0 0.0 1.0
  (True, True) -> GL.Color3 0.5 0.5 0.5

verticesFromFaces :: Faces3D Bool -> [(GL.Vertex3 GL.GLfloat, GL.Color3 GL.GLfloat, GL.TexCoord2 GL.GLfloat)]
verticesFromFaces (X x) = do
  y <- [False, True]
  z <- [False, True]
  let z' = if x then not z else z
  pure
    ( toVtx $ vtxCoord <$> Vertices3D{..}
    -- , texCoordsToColor z' y
    , if x
        then GL.Color3 1.0 0.0 0.0
        else GL.Color3 1.0 0.5 0.0

    , texCoords z' y
    )
verticesFromFaces (Z z) = do
  x <- [False, True]
  y <- [False, True]
  let x' = if z then x else not x
  pure
    ( toVtx $ vtxCoord <$> Vertices3D{..}
    -- , texCoordsToColor x' y
    , if z
        then GL.Color3 0.0 1.0 0.0
        else GL.Color3 0.0 1.0 1.0
    , texCoords x' y
    )
verticesFromFaces (Y y) = do
  x <- [False, True]
  z <- [False, True]
  let x' = if y then x else not x
  pure
    ( toVtx $ vtxCoord <$> Vertices3D{..}
    -- , texCoordsToColor x' z
    , if y
        then GL.Color3 0.0 0.0 1.0
        else GL.Color3 0.5 0.0 1.0
    , texCoords x' z
    )

main :: IO ()
main = do
  let width = 640
      height = 480

  withWindow width height "GLFW-b-demo" $ \win ->
    withEventChann win $ \eventQueue -> do
      let vertices = cubeFaces >>= verticesFromFaces
          indices :: [GL.GLuint] = [0.. (length cubeFaces - 1)] >>= \i ->
            (fromIntegral (i * 4) +) <$>
            [ 0, 1, 2
            , 1, 2, 3
            ]

      vboWitness <-
        registerBuffer @VBO
          GL.ArrayBuffer
          GL.StaticDraw
          vertices

      registerLayout @VBO

      indexWitness <-
        registerBuffer @GL.GLuint
          GL.ElementArrayBuffer
          GL.StaticDraw
          indices

      GL.blend $= GL.Enabled
      GL.blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)

      program <-
        loadShaders
          [ ShaderInfo GL.VertexShader (FileSource "shaders/example/vert.glsl")
          , ShaderInfo GL.FragmentShader (FileSource "shaders/example/frag.glsl")
          ]

      -- NOTE: this sucks really bad, but `GLUtil` enables the texture
      let wall = either error id <$> GLUtil.readTexture "assets/wall.jpg"
          smiley =
            either error id
              <$> readTexInfoFlipped "assets/wiener.png" GLUtil.loadTexture

      textures <- registerTextures [(GL.Texture2D, wall), (GL.Texture2D, smiley)]

      GL.currentProgram $= Just program

      -- TODO: attach uniforms to the "shader" abstraction
      forM_ textures $ \texUnit@(GL.TextureUnit t) -> do
        print ("texture" <> show t)
        tex <- GL.get $ GL.uniformLocation program ("texture" <> show t)
        print tex
        GL.uniform tex $= texUnit

      -- Set wireframe mode
      -- GL.polygonMode $= (GL.Line, GL.Line)

      let env = KlareEnv
                  { events = eventQueue
                  , window = win
                  }
          state = KlareState
                  { winHeight = height
                  , winWidth = width
                  , cursorInWindow = False
                  , mouseButtonPressed = False
                  , cameraPos   = V3 0.0 0.0 3.0
                  , cameraFront = V3 0.0 0.0 (-1.0)
                  , cameraUp    = V3 0.0 1.0 0.0
                  , yaw         = -90.0
                  , pitch       = 0.0
                  , lastX       = Nothing
                  , lastY       = Nothing
                  , deltaTime   = 0.0
                  , lastFrame   = 0.0
                  }

      GL.depthFunc $= Just GL.Less
      void
        $ flip runStateT state
        $ flip runReaderT env
        $ renderLoop (fromIntegral $ length indices) program

renderLoop :: GL.NumArrayIndices -> GL.Program -> KlareM ()
renderLoop numIndices program = do
  prevTime <- gets lastFrame
  time <- maybe prevTime double2Float <$> liftIO GLFW.getTime
  modify $ \s ->
    s { deltaTime = time - prevTime
      , lastFrame = time
      }
  win <- asks window
  eventQueue <- asks events

  let
    positions :: [V3 GL.GLfloat] =
      [ V3   0.0    0.0    0.0
      , V3   2.0    5.0  (-15.0)
      , V3 (-1.5) (-2.2) (-2.5)
      , V3 (-3.8) (-2.0) (-12.3)
      , V3   2.4  (-0.4) (-3.5)
      , V3 (-1.7)   3.0  (-7.5)
      , V3   1.3  (-2.0) (-2.5)
      , V3   1.5    2.0  (-2.5)
      , V3   1.5    0.2  (-1.5)
      , V3 (-1.3)   1.0  (-1.5)
      ]

  KlareState{..} <- get
  u_Model <- GL.get $ GL.uniformLocation program "u_Model"
  u_View <- GL.get $ GL.uniformLocation program "u_View"
  u_Projection <- GL.get $ GL.uniformLocation program "u_Projection"

  let 
      w = fromIntegral winWidth
      h = fromIntegral winHeight
      radius = 15.0
      camX = sin time * radius
      camZ = cos time * radius
      view :: M44 GL.GLfloat =
        lookAt
          cameraPos -- Eye position
          (cameraPos + cameraFront) -- Scene origin
          cameraUp -- Up orientation (i.e. positive y axis)
      projection :: M44 GL.GLfloat = perspective (toRadian 45.0) (w / h) 0.1 100.0
      -- view :: M44 GL.GLfloat = identity
      -- projection :: M44 GL.GLfloat = identity
  liftIO $ GLUtil.asUniform view u_View
  liftIO $ GLUtil.asUniform projection u_Projection

  GL.clearColor $= GL.Color4 0.2 0.3 0.3 1
  liftIO $ GL.clear [GL.ColorBuffer, GL.DepthBuffer]
  forM_ (zip positions [0..]) $ \(pos, idx) -> do
    let
        rot :: M44 GL.GLfloat = m33_to_m44 $ fromQuaternion $ axisAngle (V3 1.0 0.3 0.5) (toRadian (20 * idx) + time)
        trans = translate pos
        model = trans !*! rot
        -- model :: M44 GL.GLfloat = identity
    liftIO $ GLUtil.asUniform model u_Model
    liftIO $ handleGLErrors $ GL.drawElements GL.Triangles numIndices GL.UnsignedInt nullPtr


  liftIO $ do
    GLFW.swapBuffers win
    GLFW.pollEvents

  handleEvents eventQueue handler

  q <- liftIO $ GLFW.windowShouldClose win
  unless q $ renderLoop numIndices program

resizeWindow :: Int -> Int -> KlareM ()
resizeWindow w h = do
  modify $ \s -> s { winHeight = h, winWidth = w }
  liftIO $ do
    GL.viewport $= (GL.Position 0 0, GL.Size (fromIntegral w) (fromIntegral h))
    GL.matrixMode $= GL.Projection
    GL.loadIdentity
    -- GL.ortho2D 0 (realToFrac w) (realToFrac h) 0

handler :: Event -> KlareM ()
handler = \case
  (EventError e s) ->
    printEvent "error" [show e, show s]
  (EventWindowPos _ x y) ->
    printEvent "window pos" [show x, show y]
  (EventWindowSize _ width height) -> do
    printEvent "window size" [show width, show height]
    resizeWindow width height
  (EventWindowClose window) -> do
    printEvent "window close" []
    shutdown window
  (EventWindowRefresh _) ->
    printEvent "window refresh" []
  (EventWindowFocus _ fs) ->
    printEvent "window focus" [show fs]
  (EventWindowIconify _ is) ->
    printEvent "window iconify" [show is]
  (EventFramebufferSize _ width height) ->
    printEvent "framebuffer size" [show width, show height]
  (EventMouseButton _ mb mbs mk) -> do
    printEvent "mouse button" [show mb, show mbs, showModifierKeys mk]
    modify $ \s -> 
      s { mouseButtonPressed = case mbs of
                                GLFW.MouseButtonState'Pressed -> True
                                GLFW.MouseButtonState'Released -> False
        }
  (EventCursorPos _ x' y') -> do
    printEvent "cursor pos" [show x', show y']
    let x = double2Float x'
        y = double2Float y'
    modify $ \s -> 
      case (lastX s, lastY s, cursorInWindow s, mouseButtonPressed s) of
        (Just lx, Just ly, True, True) ->
          let
              xOffset = x - lx
              yOffset = y - ly
              sensitivity = 0.3
              dirX yaw pitch = cos (toRadian yaw) * cos (toRadian pitch)
              dirZ yaw pitch = sin (toRadian yaw) * cos (toRadian pitch)
              dirY pitch = sin . toRadian $ pitch
              newYaw = yaw s - (xOffset * sensitivity)
              newPitch = clamp (-89.0, 89.1) $ pitch s + (yOffset * sensitivity)
          in s 
              { lastX = Just x
              , lastY = Just y 
              , yaw = newYaw
              , pitch = newPitch
              , cameraFront = normalize $ V3 (dirX newYaw newPitch) (dirY newPitch) (dirZ newYaw newPitch)
              }
        (Nothing, Nothing, True, _) -> 
          s 
            { lastX = Just x
            , lastY = Just y
            }
        (Just lx, Just ly, True, False) ->
          s 
            { lastX = Just x
            , lastY = Just y
            }
        (Just lx, Just ly, False, _) ->
          s 
            { lastX = Nothing
            , lastY = Nothing
            }
        (Nothing, Nothing, False, _) -> s
  (EventCursorEnter _ cs) -> do
    printEvent "cursor enter" [show cs]
    modify $ \s -> 
      s 
        { cursorInWindow = case cs of
                            GLFW.CursorState'InWindow -> True
                            GLFW.CursorState'NotInWindow -> False
        }
  (EventScroll _ x y) ->
    printEvent "scroll" [show x, show y]
  (EventKey win k scancode ks mk) -> do
    printEvent "key" [show k, show scancode, show ks, showModifierKeys mk]
    dTime <- gets deltaTime
    let cameraSpeed = pure $ 3.5 * dTime
    case (ks, k) of
      (GLFW.KeyState'Pressed, GLFW.Key'Q) ->
        liftIO $ GLFW.setWindowShouldClose win True
      (GLFW.KeyState'Pressed, GLFW.Key'Escape) ->
        liftIO $ GLFW.setWindowShouldClose win True
      (GLFW.KeyState'Pressed, GLFW.Key'I) ->
        liftIO $ printInformation win
      (GLFW.KeyState'Pressed, GLFW.Key'W) ->
        modify (\s -> s { cameraPos = cameraPos s + cameraFront s * cameraSpeed })
      (GLFW.KeyState'Repeating, GLFW.Key'W) ->
        modify (\s -> s { cameraPos = cameraPos s + cameraFront s * cameraSpeed })
      (GLFW.KeyState'Pressed, GLFW.Key'S) ->
        modify (\s -> s { cameraPos = cameraPos s - cameraFront s * cameraSpeed })
      (GLFW.KeyState'Repeating, GLFW.Key'S) ->
        modify (\s -> s { cameraPos = cameraPos s - cameraFront s * cameraSpeed })
      (GLFW.KeyState'Pressed, GLFW.Key'A) ->
        modify (\s -> s { cameraPos = cameraPos s - normalize (cross (cameraFront s) (cameraUp s)) * cameraSpeed })
      (GLFW.KeyState'Repeating, GLFW.Key'A) ->
        modify (\s -> s { cameraPos = cameraPos s - normalize (cross (cameraFront s) (cameraUp s)) * cameraSpeed })
      (GLFW.KeyState'Pressed, GLFW.Key'D) ->
        modify (\s -> s { cameraPos = cameraPos s + normalize (cross (cameraFront s) (cameraUp s)) * cameraSpeed })
      (GLFW.KeyState'Repeating, GLFW.Key'D) ->
        modify (\s -> s { cameraPos = cameraPos s + normalize (cross (cameraFront s) (cameraUp s)) * cameraSpeed })
      _ -> pure ()
  (EventChar _ c) ->
    printEvent "char" [show c]

shutdown :: MonadIO m => GLFW.Window -> m ()
shutdown win = liftIO $ do
  GLFW.destroyWindow win
  GLFW.terminate
  exitSuccess

withWindow :: Int -> Int -> String -> (GLFW.Window -> IO ()) -> IO ()
withWindow width height title f = do
  GLFW.setErrorCallback $ Just simpleErrorCallback
  r <- GLFW.init
  GLFW.defaultWindowHints
  when r $ do
    m <- GLFW.createWindow width height title Nothing Nothing
    case m of
      (Just win) -> do
        printInformation win
        GLFW.makeContextCurrent m
        f win
        shutdown win
      Nothing -> GLFW.terminate
 where
  simpleErrorCallback e s =
    putStrLn $ unwords [show e, show s]
