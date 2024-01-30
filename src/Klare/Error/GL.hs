module Klare.Error.GL where

import Control.Exception
import GHC.Stack
import Graphics.Rendering.OpenGL.GLU.Errors (errors)


-- TODO: Improve this error handling machinery, possibly instrument with 
-- source locations
data GLException = GLException deriving Show

instance Exception GLException

handleGLErrors :: HasCallStack => IO () -> IO ()
handleGLErrors action = do
  errors -- Clear erros
  action
  es <- errors
  case es of
    [] -> pure ()
    es -> do 
      putStrLn (prettyCallStack callStack)
      print es 
      throwIO GLException
