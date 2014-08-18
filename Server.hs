{-# LANGUAGE DeriveFunctor, MonadComprehensions, DeriveDataTypeable, TypeSynonymInstances, FlexibleInstances #-}
import           Control.Concurrent.STM
import           Control.Exception
import           Control.Monad
import           Control.Monad.State
import qualified Data.Map      as M
import           Data.Map (Map)
import           Data.Monoid
import           Data.Typeable
import           Network
import           Pipes
import           Pipes.Concurrent
import qualified Pipes.Prelude as P
import           System.IO

import           Op

data Signed a =
  Signed { sign :: ClientID
         , thing :: a }
  deriving Functor

type ServerST = Map ClientID [Delta]
type Server m = Pipe (Signed TrackedDelta)  -- ^ tracked delta     (from all client readers)
                     (Signed Delta)         -- ^ transformed delta (to broadcaster)
                     (StateT ServerST m)    -- ^ client histories
                     ()

type ClientID = Int
data Client   = Client ClientID (Output (TrackedDelta))

-- | Message type received by broadcaster
data Broadcast
  = FromServer   (Signed Delta) -- ^ transformed delta from server
  | FromListener Client         -- ^ new client from listener

type BroadcastST
  = Map ClientID (Int, (Output TrackedDelta))

data Disconnect = Disconnect
  deriving (Typeable, Show)
instance Exception Disconnect

server :: Monad m => Server m
server = do
  (Signed s (TrackedDelta c d)) <- await
  m <- lift $ get
  let Just hist = M.lookup s m
  let (d', hist') = update d c hist
  lift $ modify (M.insert s hist')
  yield (Signed s d')

reader :: MonadIO m => ClientID -> Handle -> Producer (Signed TrackedDelta) m ()
reader cid h = P.fromHandle h >-> P.read >-> P.map (Signed cid)

writer :: MonadIO m => Handle -> Consumer TrackedDelta m ()
writer h = P.map (show . delta) >-> P.toHandle h

listen :: Output (Signed TrackedDelta) -> Handle -> StateT ClientID IO Client
listen readerout h = do
  cid <- get
  modify (+1)
  lift $ do
    (writerout, writerin, writerseal) <- spawn' Unbounded
    writerid <- forkIO $ catch (runEffect $ fromInput writerin >-> writer h)
                               (\Disconnect -> atomically $ writerseal)
    forkIO $ do runEffect $ reader cid h >-> toOutput readerout
                throwTo writerid Disconnect
    return $ Client cid writerout

listener :: Output (Signed TrackedDelta) -> Socket -> Producer Client (StateT ClientID IO) ()
listener readerout sock = do
  (h,  _, _) <- liftIO $ accept sock
  yield =<< (lift $ listen readerout h)

broadcaster :: Output Delta -> Consumer Broadcast (StateT BroadcastST IO) ()
broadcaster snapshotout = await >>= \x -> case x of
  FromListener (Client s writerout) -> lift (modify (M.insert s (0, writerout)))
  FromServer   sd                   -> void $ lift $ do
    lift $ atomically $ send snapshotout (thing sd)
    m  <- get
    m' <- lift $ M.traverseWithKey (sendEach sd) m
    put m'
  where sendEach sd cid (x, mailbox)
          | sign sd == cid = return (x + 1, mailbox)
          | otherwise      = do atomically $ send mailbox
                                           $ TrackedDelta x $ thing sd
                                return (0, mailbox)
type Snapshot = TVar Delta

snapshotter :: Snapshot -> Consumer Delta IO ()
snapshotter s = do
  x    <- await
  lift $  atomically $ modifyTVar s (<>x)

main :: IO ()
main = do
  sock                        <- listenOn (PortNumber 9999)
  (readerout, readerin)       <- spawn Unbounded -- server's mailbox for clients to use
  (broadcastout, broadcastin) <- spawn Unbounded -- broadcaster's mailbox for server and listener to use
  (snapshotout, snapshotin)   <- spawn Unbounded -- snapshotter's mailbox for broadcaster to use
  snapshot                    <- atomically $ newTVar (mempty::Delta)
  -- server
  forkIO $ void $ flip runStateT M.empty $ runEffect
         $   fromInput readerin    -- ops from client readers
         >-> server                -- ops transform
         >-> P.map FromServer
         >-> toOutput broadcastout -- send to broadcaster
  -- broadcast
  forkIO $ void $ flip runStateT M.empty $ runEffect
         $   fromInput broadcastin   -- messsages from server and listener
         >-> broadcaster snapshotout -- broadcast & take snapshot
  -- snapshot
  forkIO $ runEffect
         $   fromInput snapshotin
         >-> snapshotter snapshot
  -- listener (for new clients)
  void $ flip runStateT 0 $ runEffect
       $   listener readerout sock -- give the server's mailbox to new clients
       >-> P.map FromListener
       >-> toOutput broadcastout   -- let broadcaster know about the new client
