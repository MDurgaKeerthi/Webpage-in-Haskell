{-# OPTIONS_GHC -fno-warn-orphans #-}
module Application
    ( getApplicationDev       --  for DevelMain
    , appMain
    , develMain
    , makeFoundation
    , makeLogWare
    , getApplicationRepl
    , shutdownApp
    , handler     --for GHCI
    , db
    ) where

import Control.Monad.Logger                 (liftLoc, runLoggingT)
import Database.Persist.Sqlite              (createSqlitePool, runSqlPool,
                                             sqlDatabase, sqlPoolSize)
import Import
import Language.Haskell.TH.Syntax           (qLocation)
import Network.Wai (Middleware)
import Network.Wai.Handler.Warp             (Settings, defaultSettings,
                                             defaultShouldDisplayException,
                                             runSettings, setHost,
                                             setOnException, setPort, getPort)
import Network.Wai.Middleware.RequestLogger (Destination (Logger),
                                             IPAddrSource (..),
                                             OutputFormat (..), destination,
                                             mkRequestLogger, outputFormat)
import System.Log.FastLogger                (defaultBufSize, newStdoutLoggerSet,
                                             toLogStr)

-- Importing all handler modules
import Handler.Home
import Handler.Opinion
import Handler.Profile
import Handler.Transaction
import Handler.Checkpassword
import Handler.Allowtrans
import Handler.Tryagain
import Handler.Suc_trans
import Handler.Suc_change
import Handler.Details
import Handler.Contact
import Handler.Feedback
import Handler.Paybill
import Handler.Suc_bill
import Handler.Instructions

mkYesodDispatch "App" resourcesApp  --instance for yesod dispatch. second half of mkYesodData call in Foundation.hs. 

makeFoundation :: AppSettings -> IO App   --allocates resources (database connection pool),performs initialization and returns a foundation datatype value.
makeFoundation appSettings = do
    appHttpManager <- newManager          --initializations
    appLogger <- newStdoutLoggerSet defaultBufSize >>= makeYesodLogger
    appStatic <-
        (if appMutableStatic appSettings then staticDevel else static)
        (appStaticDir appSettings)

    let mkFoundation appConnPool = App {..}
        tempFoundation = mkFoundation $ error "connPool forced in tempFoundation"
        logFunc = messageLoggerSource tempFoundation appLogger

    -- creating the database connection pool
    pool <- flip runLoggingT logFunc $ createSqlitePool
        (sqlDatabase $ appDatabaseConf appSettings)
        (sqlPoolSize $ appDatabaseConf appSettings)

    runLoggingT (runSqlPool (runMigration migrateAll) pool) logFunc

    return $ mkFoundation pool

makeApplication :: App -> IO Application             -- converts foundation to a WAI Application
makeApplication foundation = do
    logWare <- makeLogWare foundation                -- creating the WAI application and apply middlewares
    appPlain <- toWaiAppPlain foundation
    return $ logWare $ defaultMiddlewaresNoLogging appPlain

makeLogWare :: App -> IO Middleware
makeLogWare foundation =
    mkRequestLogger def
        { outputFormat =
            if appDetailedRequestLogging $ appSettings foundation
                then Detailed True
                else Apache
                        (if appIpFromHeader $ appSettings foundation
                            then FromFallback
                            else FromSocket)
        , destination = Logger $ loggerSet $ appLogger foundation
        }


--  Warp settings for this temporary foundation
warpSettings :: App -> Settings
warpSettings foundation =
      setPort (appPort $ appSettings foundation)
    $ setHost (appHost $ appSettings foundation)
    $ setOnException (\_req e ->
        when (defaultShouldDisplayException e) $ messageLoggerSource
            foundation
            (appLogger foundation)
            $(qLocation >>= liftLoc)
            "yesod"
            LevelError
            (toLogStr $ "Exception from Warp: " ++ show e))
      defaultSettings

-- for yesod devel, return the Warp settings and WAI Application.
getApplicationDev :: IO (Settings, Application)
getApplicationDev = do
    settings <- getAppSettings
    foundation <- makeFoundation settings
    wsettings <- getDevSettings $ warpSettings foundation
    app <- makeApplication foundation
    return (wsettings, app)

getAppSettings :: IO AppSettings    --loading yamlsettings
getAppSettings = loadYamlSettings [configSettingsYml] [] useEnv

-- for yesod devel
develMain :: IO ()
develMain = develMainHelper getApplicationDev

-- function for an executable running this site, used in main.hs
appMain :: IO ()
appMain = do
    settings <- loadYamlSettingsArgs
        [configSettingsYmlValue]
        useEnv

    foundation <- makeFoundation settings

    app <- makeApplication foundation

    runSettings (warpSettings foundation) app


-- for DevelMain.hs (a way to run the app from GHCi)
getApplicationRepl :: IO (Int, App, Application)
getApplicationRepl = do
    settings <- getAppSettings
    foundation <- makeFoundation settings
    wsettings <- getDevSettings $ warpSettings foundation
    app1 <- makeApplication foundation
    return (getPort wsettings, foundation, app1)

shutdownApp :: App -> IO ()
shutdownApp _ = return ()


--for use in development with GHCi

-- run a handler
handler :: Handler a -> IO a
handler h = getAppSettings >>= makeFoundation >>= flip unsafeHandler h

-- | run DB queries
db :: ReaderT SqlBackend (HandlerT App IO) a -> IO a
db = handler . runDB
