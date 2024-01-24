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

bufferOffset :: (Integral a) => a -> Ptr b
bufferOffset = plusPtr nullPtr . fromIntegral

data Descriptor = Descriptor GL.VertexArrayObject GL.ArrayIndex GL.NumArrayIndices

main :: IO ()
main = do
  let width = 640
      height = 480

  withWindow width height "GLFW-b-demo" $ \win -> do
    let vertices :: [GL.Vertex2 GL.GLfloat]
        vertices =
          [ GL.Vertex2 (-0.5) (-0.5)
          , GL.Vertex2 0.0 0.5
          , GL.Vertex2 0.5 (-0.5)
          ]
        numVertices = length vertices

    vertexBuffer <- GL.genObjectName
    GL.bindBuffer GL.ArrayBuffer $= Just vertexBuffer

    withArray vertices $ \ptr -> do
      let sizev = fromIntegral (numVertices * sizeOf (head vertices))
      GL.bufferData GL.ArrayBuffer $= (sizev, ptr, GL.StaticDraw)

    let attribLoc = GL.AttribLocation 0
        firstIndex = 0
        vtxDesc =
          GL.VertexArrayDescriptor
            2
            GL.Float
            0
            (bufferOffset firstIndex)

    GL.vertexAttribPointer attribLoc $= (GL.ToFloat, vtxDesc)
    GL.vertexAttribArray attribLoc $= GL.Enabled

    program <-
      loadShaders
        [ ShaderInfo GL.VertexShader (FileSource "shaders/example/vert.glsl")
        , ShaderInfo GL.FragmentShader (FileSource "shaders/example/frag.glsl")
        ]

    GL.currentProgram $= Just program
    renderLoop win

renderLoop :: GLFW.Window -> IO ()
renderLoop win = do
  GL.clearColor $= GL.Color4 0 0 0 1
  GL.clear [GL.ColorBuffer]
  GL.drawArrays GL.Triangles 0 3
  GLFW.swapBuffers win

  q <- GLFW.windowShouldClose win
  unless q $ do
    GLFW.pollEvents
    renderLoop win

resizeWindow :: GLFW.WindowSizeCallback
resizeWindow win w h =
  do
    GL.viewport $= (GL.Position 0 0, GL.Size (fromIntegral w) (fromIntegral h))
    GL.matrixMode $= GL.Projection
    GL.loadIdentity
    GL.ortho2D 0 (realToFrac w) (realToFrac h) 0

keyPressed :: GLFW.KeyCallback
keyPressed win GLFW.Key'Escape _ GLFW.KeyState'Pressed _ = shutdown win
keyPressed _ _ _ _ _ = return ()

shutdown :: GLFW.WindowCloseCallback
shutdown win = do
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
        GLFW.makeContextCurrent m
        GLFW.setWindowSizeCallback win (Just resizeWindow)
        GLFW.setKeyCallback win (Just keyPressed)
        GLFW.setWindowCloseCallback win (Just shutdown)
        f win
        GLFW.destroyWindow win
      Nothing -> return ()
    GLFW.terminate
 where
  simpleErrorCallback e s =
    putStrLn $ unwords [show e, show s]
