module Klare.Events.Callbacks where

import Klare.Events.Type
import Control.Concurrent.STM (TQueue, atomically, newTQueueIO, tryReadTQueue, writeTQueue)
import Graphics.UI.GLFW qualified as GLFW


charCallback :: TQueue Event -> GLFW.Window -> Char -> IO ()
charCallback tc win c = atomically $ writeTQueue tc $ EventChar win c

cursorEnterCallback :: TQueue Event -> GLFW.Window -> GLFW.CursorState -> IO ()
cursorEnterCallback tc win ca = atomically $ writeTQueue tc $ EventCursorEnter win ca

cursorPosCallback :: TQueue Event -> GLFW.Window -> Double -> Double -> IO ()
cursorPosCallback tc win x y = atomically $ writeTQueue tc $ EventCursorPos win x y

errorCallback :: TQueue Event -> GLFW.Error -> String -> IO ()
errorCallback tc e s = atomically $ writeTQueue tc $ EventError e s

framebufferSizeCallback :: TQueue Event -> GLFW.Window -> Int -> Int -> IO ()
framebufferSizeCallback tc win w h = atomically $ writeTQueue tc $ EventFramebufferSize win w h

keyCallback :: TQueue Event -> GLFW.Window -> GLFW.Key -> Int -> GLFW.KeyState -> GLFW.ModifierKeys -> IO ()
keyCallback tc win k sc ka mk = atomically $ writeTQueue tc $ EventKey win k sc ka mk

mouseButtonCallback :: TQueue Event -> GLFW.Window -> GLFW.MouseButton -> GLFW.MouseButtonState -> GLFW.ModifierKeys -> IO ()
mouseButtonCallback tc win mb mba mk = atomically $ writeTQueue tc $ EventMouseButton win mb mba mk

scrollCallback :: TQueue Event -> GLFW.Window -> Double -> Double -> IO ()
scrollCallback tc win x y = atomically $ writeTQueue tc $ EventScroll win x y

windowCloseCallback :: TQueue Event -> GLFW.Window -> IO ()
windowCloseCallback tc win = atomically $ writeTQueue tc $ EventWindowClose win

windowFocusCallback :: TQueue Event -> GLFW.Window -> Bool -> IO ()
windowFocusCallback tc win fa = atomically $ writeTQueue tc $ EventWindowFocus win fa

windowIconifyCallback :: TQueue Event -> GLFW.Window -> Bool -> IO ()
windowIconifyCallback tc win ia = atomically $ writeTQueue tc $ EventWindowIconify win ia

windowPosCallback :: TQueue Event -> GLFW.Window -> Int -> Int -> IO ()
windowPosCallback tc win x y = atomically $ writeTQueue tc $ EventWindowPos win x y

windowRefreshCallback :: TQueue Event -> GLFW.Window -> IO ()
windowRefreshCallback tc win = atomically $ writeTQueue tc $ EventWindowRefresh win

windowSizeCallback :: TQueue Event -> GLFW.Window -> Int -> Int -> IO ()
windowSizeCallback tc win w h = atomically $ writeTQueue tc $ EventWindowSize win w h
