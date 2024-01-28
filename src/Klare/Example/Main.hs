module Klare.Example.Main where

import Control.Monad
import Foreign (Storable (..))
import Foreign.Marshal.Array (withArray)
import Foreign.Ptr (Ptr, nullPtr, plusPtr)
import Graphics.Rendering.OpenGL (($=))
import Graphics.Rendering.OpenGL qualified as GL
import Graphics.UI.GLFW qualified as GLFW
import Shaders.Load
import System.Exit (exitSuccess)
import Klare.Events.Type
import Control.Concurrent.STM.TQueue
import Klare.Events (handleEvents, withEventChann)
import Klare.Info.Print
import Data.Monoid (Sum(..))
import Control.Exception (throwIO)
import Control.Exception.Base (Exception)
import GHC.Float (double2Float)
import Foreign.Storable.Tuple ()
import Data.Bifunctor
import GHC.Stack (HasCallStack, callStack)
import GHC.Exception (prettyCallStack)
import Graphics.GLUtil qualified as GLUtil

bufferOffset :: (Integral a) => a -> Ptr b
bufferOffset = plusPtr nullPtr . fromIntegral

data Descriptor = Descriptor GL.VertexArrayObject GL.ArrayIndex GL.NumArrayIndices

main :: IO ()
main = do
  let width = 640
      height = 480

  withWindow width height "GLFW-b-demo" $ \win -> 
    withEventChann win $ \eventQueue -> do
      let shapeData :: [(GL.Vertex3 GL.GLfloat, GL.Color3 GL.GLfloat, GL.TexCoord2 GL.GLfloat)]
          shapeData =
            [ (GL.Vertex3 (-0.5) (-0.5) (-1), GL.Color3 1.0 0.0 0.0, GL.TexCoord2 0.0 0.0)
            , (GL.Vertex3 0.5 (-0.5) (-1)   , GL.Color3 0.0 1.0 0.0, GL.TexCoord2 1.0 0.0)
            , (GL.Vertex3 (-0.5) 0.5 1      , GL.Color3 0.0 0.0 1.0, GL.TexCoord2 0.0 1.0)
            , (GL.Vertex3 0.5 0.5 1         , GL.Color3 0.5 0.5 0.5, GL.TexCoord2 1.0 1.0)
            ]

          indices :: [GL.GLuint]
          indices = [ 0, 1, 2
                    , 1, 2, 3
                    ]

      vertexBuffer <- GL.genObjectName
      GL.bindBuffer GL.ArrayBuffer $= Just vertexBuffer
      
      withArray shapeData $ \ptr -> do
        let sizev = 
                fromIntegral 
              . getSum 
              $ foldMap (Sum . sizeOf) 
                shapeData
        GL.bufferData GL.ArrayBuffer $= (sizev, ptr, GL.StaticDraw)

      let stride = fromIntegral . sizeOf $ head shapeData

          vtxAttribLoc = GL.AttribLocation 0
          vtxFirstIndex = 0
          vtxDesc =
            GL.VertexArrayDescriptor
              3
              GL.Float
              stride
              (bufferOffset vtxFirstIndex)

          clrAttribLoc = GL.AttribLocation 1
          clrFirstIndex = fromIntegral . sizeOf . (\(a,_,_) -> a) $ head shapeData
          clrDesc =
            GL.VertexArrayDescriptor
              3
              GL.Float
              stride
              (bufferOffset clrFirstIndex)

          texAttribLoc = GL.AttribLocation 2
          texFirstIndex = fromIntegral . sizeOf . (\(a,b,_) -> (a,b)) $ head shapeData
          texDesc =
            GL.VertexArrayDescriptor
              2
              GL.Float
              stride
              (bufferOffset texFirstIndex)

      GL.vertexAttribPointer vtxAttribLoc $= (GL.ToFloat, vtxDesc)
      GL.vertexAttribArray vtxAttribLoc $= GL.Enabled

      GL.vertexAttribPointer clrAttribLoc $= (GL.ToFloat, clrDesc)
      GL.vertexAttribArray clrAttribLoc $= GL.Enabled

      GL.vertexAttribPointer texAttribLoc $= (GL.ToFloat, texDesc)
      GL.vertexAttribArray texAttribLoc $= GL.Enabled

      indexBuffer <- GL.genObjectName
      GL.bindBuffer GL.ElementArrayBuffer $= Just indexBuffer

      withArray indices $ \ptr -> do
        let sizev = fromIntegral . getSum $ foldMap (Sum . sizeOf) indices
        GL.bufferData GL.ElementArrayBuffer $= (sizev, ptr, GL.StaticDraw)

      tx <- either error id <$> GLUtil.readTexture "assets/wall.jpg"
      GL.textureFilter GL.Texture2D $= ((GL.Linear', Nothing), GL.Linear')
      GLUtil.texture2DWrap $= (GL.Repeated, GL.ClampToEdge)
      GL.texture GL.Texture2D $= GL.Enabled
      GL.activeTexture $= GL.TextureUnit 0
      GL.textureBinding GL.Texture2D $= Just tx

      program <-
        loadShaders
          [ ShaderInfo GL.VertexShader (FileSource "shaders/example/vert.glsl")
          , ShaderInfo GL.FragmentShader (FileSource "shaders/example/frag.glsl")
          ]

      GL.currentProgram $= Just program
      -- Set wireframe mode
      -- GL.polygonMode $= (GL.Line, GL.Line)
      renderLoop win program eventQueue

renderLoop :: GLFW.Window -> GL.Program ->  TQueue Event -> IO ()
renderLoop win program eventQueue = do
  time <- GLFW.getTime
  let gv = maybe 0.3 ((\t -> (sin t / 2.0) + 0.5) . double2Float) time
  u_Colour <- GL.get $ GL.uniformLocation program "u_Colour"
  when (u_Colour /= GL.UniformLocation (-1)) $
    GL.uniform u_Colour $= (GL.Color4 @GL.GLfloat) 0.3 gv 0.2 0.1
  draw
  GLFW.swapBuffers win
  GLFW.pollEvents
  handleEvents eventQueue handler

  q <- GLFW.windowShouldClose win
  unless q $ renderLoop win program eventQueue

draw :: IO ()
draw = do
  GL.clearColor $= GL.Color4 0.2 0.3 0.3 1
  GL.clear [GL.ColorBuffer]
  handleGLErrors $ GL.drawElements GL.Triangles 6 GL.UnsignedInt nullPtr

resizeWindow :: GLFW.WindowSizeCallback
resizeWindow win w h =
  do
    GL.viewport $= (GL.Position 0 0, GL.Size (fromIntegral w) (fromIntegral h))
    GL.matrixMode $= GL.Projection
    GL.loadIdentity
    GL.ortho2D 0 (realToFrac w) (realToFrac h) 0

handler :: Event -> IO ()
handler = \case
  (EventError e s) -> 
    printEvent "error" [show e, show s]
  (EventWindowPos _ x y) ->
    printEvent "window pos" [show x, show y]
  (EventWindowSize window width height) -> do
    printEvent "window size" [show width, show height]
    resizeWindow window width height
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
          GLFW.setWindowShouldClose win True
      -- i: print GLFW information
      when (k == GLFW.Key'I) $
        printInformation win
  (EventChar _ c) ->
    printEvent "char" [show c]

shutdown :: GLFW.WindowCloseCallback
shutdown win = do
  GLFW.destroyWindow win
  GLFW.terminate
  exitSuccess

-- TODO: Improve this error handling machinery, possibly instrument with 
-- source locations
data GLException = GLException deriving Show

instance Exception GLException

handleGLErrors :: HasCallStack => IO () -> IO ()
handleGLErrors action = do
  GL.errors
  action
  es <- GL.errors
  case es of
    [] -> pure ()
    es -> do 
      putStrLn (prettyCallStack callStack)
      print es 
      throwIO GLException

withWindow :: Int -> Int -> String -> (GLFW.Window -> IO ()) -> IO ()
withWindow width height title f = do
  GLFW.setErrorCallback $ Just simpleErrorCallback
  r <- GLFW.init
  GLFW.defaultWindowHints
  when r $ do
    m <- GLFW.createWindow width height title Nothing Nothing
    case m of
      (Just win) -> do
        GLFW.makeContextCurrent m
        f win
        shutdown win
      Nothing -> GLFW.terminate
 where
  simpleErrorCallback e s =
    putStrLn $ unwords [show e, show s]
