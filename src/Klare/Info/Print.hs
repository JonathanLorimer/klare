module Klare.Info.Print where

import qualified Graphics.UI.GLFW as GLFW
import Text.PrettyPrint
import Data.List (intercalate)
import Data.Maybe (catMaybes)
import Control.Monad.IO.Class 

printEvent :: MonadIO m => String -> [String] -> m ()
printEvent cbname fields = liftIO $ putStrLn $ cbname ++ ": " ++ unwords fields

showModifierKeys :: GLFW.ModifierKeys -> String
showModifierKeys mk =
  "[mod keys: " ++ keys ++ "]"
 where
  keys = if null xs then "none" else unwords xs
  xs = catMaybes ys
  ys =
    [ if GLFW.modifierKeysShift mk then Just "shift" else Nothing
    , if GLFW.modifierKeysControl mk then Just "control" else Nothing
    , if GLFW.modifierKeysAlt mk then Just "alt" else Nothing
    , if GLFW.modifierKeysSuper mk then Just "super" else Nothing
    ]

printInformation :: GLFW.Window -> IO ()
printInformation win = do
  version <- GLFW.getVersion
  versionString <- GLFW.getVersionString
  clientAPI <- GLFW.getWindowClientAPI win
  cv0 <- GLFW.getWindowContextVersionMajor win
  cv1 <- GLFW.getWindowContextVersionMinor win
  cv2 <- GLFW.getWindowContextVersionRevision win
  robustness <- GLFW.getWindowContextRobustness win
  forwardCompat <- GLFW.getWindowOpenGLForwardCompat win
  debug <- GLFW.getWindowOpenGLDebugContext win
  profile <- GLFW.getWindowOpenGLProfile win

  putStrLn $
    render $
      nest
        4
        ( text "------------------------------------------------------------"
            $+$ text "GLFW C library:"
            $+$ nest
              4
              ( text "Version:"
                  <+> renderVersion version
                  $+$ text "Version string:"
                  <+> renderVersionString versionString
              )
            $+$ text "OpenGL context:"
            $+$ nest
              4
              ( text "Client API:"
                  <+> renderClientAPI clientAPI
                  $+$ text "Version:"
                  <+> renderContextVersion cv0 cv1 cv2
                  $+$ text "Robustness:"
                  <+> renderContextRobustness robustness
                  $+$ text "Forward compatibility:"
                  <+> renderForwardCompat forwardCompat
                  $+$ text "Debug:"
                  <+> renderDebug debug
                  $+$ text "Profile:"
                  <+> renderProfile profile
              )
            $+$ text "------------------------------------------------------------"
        )
 where
  renderVersion (GLFW.Version v0 v1 v2) =
    text $ intercalate "." $ map show [v0, v1, v2]

  renderVersionString =
    text . show

  renderContextVersion v0 v1 v2 =
    hcat [int v0, text ".", int v1, text ".", int v2]

  renderClientAPI = text . show
  renderContextRobustness = text . show
  renderForwardCompat = text . show
  renderDebug = text . show
  renderProfile = text . show
