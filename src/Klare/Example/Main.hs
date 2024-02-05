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
import Control.Monad.State (StateT, runStateT, put, MonadState (..))
import Control.Monad.IO.Class (MonadIO(..))
import Linear (ortho)
import Data.Ratio

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
    }

type KlareM a = ReaderT KlareEnv (StateT KlareState IO) a

type VBO = (GL.Vertex4 GL.GLfloat, GL.Color3 GL.GLfloat, GL.TexCoord2 GL.GLfloat)

main :: IO ()
main = do
  let width = 640
      height = 480

  withWindow width height "GLFW-b-demo" $ \win ->
    withEventChann win $ \eventQueue -> do
      vboWitness <-
        registerBuffer @VBO
          GL.ArrayBuffer
          GL.StaticDraw
          [ (GL.Vertex4 (-0.5) (-0.5) 0.0 1.0, GL.Color3 1.0 0.0 0.0, GL.TexCoord2 0.0 0.0)
          , (GL.Vertex4 0.5 (-0.5) 0.0 1.0, GL.Color3 0.0 1.0 0.0, GL.TexCoord2 1.0 0.0)
          , (GL.Vertex4 (-0.5) 0.5 0.0 1.0, GL.Color3 0.0 0.0 1.0, GL.TexCoord2 0.0 1.0)
          , (GL.Vertex4 0.5 0.5 0.0 1.0, GL.Color3 0.5 0.5 0.5, GL.TexCoord2 1.0 1.0)
          ]

      registerLayout @VBO

      indexWitness <-
        registerBuffer @GL.GLuint
          GL.ElementArrayBuffer
          GL.StaticDraw
          [ 0, 1, 2
          , 1, 2, 3
          ]
      
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
      let state = KlareState 
                  { winHeight = height
                  , winWidth = width
                  }
      void 
        $ flip runStateT state 
        $ flip runReaderT env 
        $ renderLoop program

renderLoop :: GL.Program -> KlareM ()
renderLoop program = do
  win <- asks window
  eventQueue <- asks events
  time <- liftIO GLFW.getTime
  let gv = maybe 0.3 ((\t -> (sin t / 2.0) + 0.5) . double2Float) time
  u_Colour <- GL.get $ GL.uniformLocation program "u_Colour"
  when (u_Colour /= GL.UniformLocation (-1)) $
    GL.uniform u_Colour $= (GL.Color4 @GL.GLfloat) 0.3 gv 0.2 0.1

  u_MVP <- GL.get $ GL.uniformLocation program "u_MVP"
  when (u_MVP /= GL.UniformLocation (-1)) $ do
    KlareState{..} <- get
    let w = fromIntegral winWidth
        h = fromIntegral winHeight
        (wRatio, hRatio) = if w > h 
                            then (1, h / w)
                            else (w / h, 1)
        proj = (ortho @GL.GLfloat) (negate wRatio) wRatio (negate hRatio) hRatio (-1.0) 1.0
    liftIO $ GLUtil.asUniform proj u_MVP 
  draw
  liftIO $ do 
    GLFW.swapBuffers win
    GLFW.pollEvents
    
  handleEvents eventQueue handler

  q <- liftIO $ GLFW.windowShouldClose win
  unless q $ renderLoop program

draw :: KlareM ()
draw = do
  GL.clearColor $= GL.Color4 0.2 0.3 0.3 1
  liftIO $ GL.clear [GL.ColorBuffer]
  liftIO $ handleGLErrors $ GL.drawElements GL.Triangles 6 GL.UnsignedInt nullPtr

resizeWindow :: Int -> Int -> KlareM ()
resizeWindow w h = do
  put $ KlareState { winHeight = h, winWidth = w } 
  liftIO $ do
    GL.viewport $= (GL.Position 0 0, GL.Size (fromIntegral w) (fromIntegral h))
    GL.matrixMode $= GL.Projection
    GL.loadIdentity
    GL.ortho2D 0 (realToFrac w) (realToFrac h) 0

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
  (EventMouseButton _ mb mbs mk) ->
    printEvent "mouse button" [show mb, show mbs, showModifierKeys mk]
  (EventCursorPos _ x y) ->
    printEvent "cursor pos" [show x, show y]
  (EventCursorEnter _ cs) ->
    printEvent "cursor enter" [show cs]
  (EventScroll _ x y) ->
    printEvent "scroll" [show x, show y]
  (EventKey win k scancode ks mk) -> do
    printEvent "key" [show k, show scancode, show ks, showModifierKeys mk]
    when (ks == GLFW.KeyState'Pressed) $ do
      -- Q, Esc: exit
      when (k == GLFW.Key'Q || k == GLFW.Key'Escape) $
        liftIO $ GLFW.setWindowShouldClose win True
      -- i: print GLFW information
      when (k == GLFW.Key'I) $
        liftIO $ printInformation win
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
