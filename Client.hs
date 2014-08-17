import qualified Graphics.UI.Threepenny       as UI
import           Graphics.UI.Threepenny.Core

main :: IO ()
main = do
  startGUI defaultConfig
      { tpPort       = Just 10000
      , tpStatic     = Just "../wwwroot"
      } setup

setup :: Window -> UI ()
setup window = do
  return window # set UI.title "Hello World!"
  doc <- UI.textarea #. "document"
  getBody window #+ [element doc]
  on UI.valueChange doc $ \s -> do
    liftIO $ putStrLn s
    return ()
