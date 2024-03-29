module Klare.Events.Type where

import qualified Graphics.UI.GLFW as GLFW

data Event
  = EventError !GLFW.Error !String
  | EventWindowPos !GLFW.Window !Int !Int
  | EventWindowSize !GLFW.Window !Int !Int
  | EventWindowClose !GLFW.Window
  | EventWindowRefresh !GLFW.Window
  | EventWindowFocus !GLFW.Window !Bool
  | EventWindowIconify !GLFW.Window !Bool
  | EventFramebufferSize !GLFW.Window !Int !Int
  | EventMouseButton !GLFW.Window !GLFW.MouseButton !GLFW.MouseButtonState !GLFW.ModifierKeys
  | EventCursorPos !GLFW.Window !Double !Double
  | EventCursorEnter !GLFW.Window !GLFW.CursorState
  | EventScroll !GLFW.Window !Double !Double
  | EventKey !GLFW.Window !GLFW.Key !Int !GLFW.KeyState !GLFW.ModifierKeys
  | EventChar !GLFW.Window !Char
  deriving (Show)

