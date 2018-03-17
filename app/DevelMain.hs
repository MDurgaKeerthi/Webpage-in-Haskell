-- running the app inside GHCi.

module DevelMain where

import Prelude
import Application (getApplicationRepl, shutdownApp)

import Control.Exception (finally)
import Control.Monad ((>=>))
import Control.Concurrent
import Data.IORef
import Foreign.Store
import Network.Wai.Handler.Warp
import GHC.Word

-- start or restart the server.
-- newStore is from foreign-store included in cabal file.  this store holds onto some data across ghci reloads
update :: IO ()
update = do
    mtidStore <- lookupStore tidStoreNum
    case mtidStore of
      Nothing -> do           -- if no server is running
          done <- storeAction doneStore newEmptyMVar
          tid <- start done
          _ <- storeAction (Store tidStoreNum) (newIORef tid)
          return ()
      Just tidStore -> restartAppInNewThread tidStore    --if server is already running
  where
    doneStore :: Store (MVar ())
    doneStore = Store 0

    restartAppInNewThread :: Store (IORef ThreadId) -> IO ()              -- killThread to restart
    restartAppInNewThread tidStore = modifyStoredIORef tidStore $ \tid -> do
        killThread tid
        withStore doneStore takeMVar
        readStore doneStore >>= start


    -- start the server in a separate thread.
    start :: MVar () 
          -> IO ThreadId
    start done = do
        (port, site, app) <- getApplicationRepl
        forkIO (finally (runSettings (setPort port defaultSettings) app)      --concurrency between shutdownApp and the next app that is starting.
                        (putMVar done () >> shutdownApp site))

-- stopping the server
shutdown :: IO ()
shutdown = do
    mtidStore <- lookupStore tidStoreNum
    case mtidStore of
      -- no server running
      Nothing -> putStrLn "no Yesod app running"
      Just tidStore -> do
          withStore tidStore $ readIORef >=> killThread
          putStrLn "Yesod app is shutdown"

tidStoreNum :: Word32
tidStoreNum = 1

modifyStoredIORef :: Store (IORef a) -> (a -> IO a) -> IO ()
modifyStoredIORef store f = withStore store $ \ref -> do
    v <- readIORef ref
    f v >>= writeIORef ref
