{-# LANGUAGE MonadComprehensions, TemplateHaskell #-}
import           Control.Applicative
import qualified Control.Lens          as L
import           Control.Lens.TH
import           Control.Monad.State.Strict
import           MVC
import qualified MVC.Prelude           as M
import           Network
import           Network.WebSockets
import qualified Pipes.Aeson.Unchecked as P
import qualified Pipes.Prelude         as P
import           System.IO
import           System.Environment
import           Op

data From = FServer TrackedDelta | FUser Delta
     deriving (Eq, Show)
data To   = TServer TrackedDelta | TUser Delta
     deriving (Eq, Show)

makePrisms ''To

main :: IO ()
main = do
  x:y:a:b:_ <- getArgs
  runServer x (read y) $ \pconn -> do
    conn      <- acceptRequest pconn
    tcph      <- connectTo a (PortNumber $ fromInteger $ read b)
    void $ runMVC (0, []) (asPipe model) (client tcph conn)

client :: Handle -> Connection -> Managed (View To, Controller From)
client h c = (,) <$> view h c <*> controller h c

model :: Pipe From To (State (Int, [Delta])) ()
model = await >>= \x -> case x of
  FServer (TrackedDelta n d) -> do
    (m, hist)       <- get
    let (d', hist') =  update d n hist
    put (m+1, hist')
    yield $ TUser d'
  FUser d -> do
    (m, hist) <- get
    put (0, hist)
    yield $ TServer (TrackedDelta m d)

controllerServer :: Handle -> Managed (Controller TrackedDelta)
controllerServer h = M.producer Single (P.fromHandle h >-> P.read)

fromConnection :: (MonadIO m, WebSocketsData a) => Connection -> Producer a m ()
fromConnection c = liftIO (receiveData c) >>= yield

toConnection :: (MonadIO m, WebSocketsData a) => Connection -> Consumer a m ()
toConnection c = await >>= liftIO . sendTextData c

controllerUser :: Connection -> Managed (Controller Delta)
controllerUser c = M.producer Single $ void $ L.view P.decoded $ fromConnection c

controller :: Handle -> Connection -> Managed (Controller From)
controller h c = fmap (fmap FServer) (controllerServer h)
              <> fmap (fmap FUser)   (controllerUser c)

viewServer :: Handle -> Managed (View TrackedDelta)
viewServer h = M.consumer (P.show >-> P.toHandle h)

viewUser :: Connection -> Managed (View Delta)
viewUser c = M.consumer (for cat P.encode >-> toConnection c)

view :: Handle -> Connection -> Managed (View To)
view h c =  fmap (handles _TServer) (viewServer h)
         <> fmap (handles _TUser)   (viewUser c)
