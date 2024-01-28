module Klare.Events where

import Graphics.UI.GLFW qualified as GLFW
import Klare.Events.Type
import Klare.Events.Callbacks
import Control.Concurrent.STM.TQueue
import Data.Foldable
import Control.Concurrent.STM (atomically)

-- | Initializes a 'TQueue' for even processing, then registers 
-- GLFW event callbacks that intercept events and write them to
-- the queue. Note, there is no cleanup necessary as the "with" name
-- might suggest, this convention merely enforces that all callbacks are
-- registered by the time the provided action that processes events is
-- called
withEventChann :: GLFW.Window -> (TQueue Event -> IO ()) -> IO ()
withEventChann win action = do
    eventsChan <- newTQueueIO :: IO (TQueue Event)

    GLFW.setErrorCallback $ Just $ errorCallback eventsChan
    GLFW.setWindowPosCallback win $ Just $ windowPosCallback eventsChan
    GLFW.setWindowSizeCallback win $ Just $ windowSizeCallback eventsChan
    GLFW.setWindowCloseCallback win $ Just $ windowCloseCallback eventsChan
    GLFW.setWindowRefreshCallback win $ Just $ windowRefreshCallback eventsChan
    GLFW.setWindowFocusCallback win $ Just $ windowFocusCallback eventsChan
    GLFW.setWindowIconifyCallback win $ Just $ windowIconifyCallback eventsChan
    GLFW.setFramebufferSizeCallback win $ Just $ framebufferSizeCallback eventsChan
    GLFW.setMouseButtonCallback win $ Just $ mouseButtonCallback eventsChan
    GLFW.setCursorPosCallback win $ Just $ cursorPosCallback eventsChan
    GLFW.setCursorEnterCallback win $ Just $ cursorEnterCallback eventsChan
    GLFW.setScrollCallback win $ Just $ scrollCallback eventsChan
    GLFW.setKeyCallback win $ Just $ keyCallback eventsChan
    GLFW.setCharCallback win $ Just $ charCallback eventsChan
    
    action eventsChan    

-- | Helper function that allows you to just case on events and handle them individually
handleEvent' :: TQueue Event -> (Event -> IO ()) ->  IO ()
handleEvent' queue f = do
  me <- atomically $ tryReadTQueue queue
  case me of
    Just e -> do
      f e
      handleEvent' queue f
    Nothing -> return ()

handleEvents :: TQueue Event -> (Event -> IO ()) ->  IO ()
handleEvents queue f = do
  es <- atomically $ flushTQueue queue
  traverse_ f es
