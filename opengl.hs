import Graphics.UI.GLUT
-- 1 
main :: IO ()
main = do
  (_progName, _args) <- getArgsAndInitialize
  _window <- createWindow "Hello World"
  displayCallback $= display
  mainLoop
 
display :: DisplayCallback
display = do
  clear [ ColorBuffer ]
  flush
