{-# Language CPP #-}

module Settings where              --contains default settings, which can be overridden, for app, server, browser, web content  

import ClassyPrelude.Yesod
import qualified Control.Exception as Exception
import Data.Aeson                  (Result (..), fromJSON, withObject, (.!=),
                                    (.:?))
import Data.FileEmbed              (embedFile)
import Data.Yaml                   (decodeEither')
import Database.Persist.Sqlite     (SqliteConf)
import Language.Haskell.TH.Syntax  (Exp, Name, Q)
import Network.Wai.Handler.Warp    (HostPreference)
import Yesod.Default.Config2       (applyEnvValue, configSettingsYml)
import Yesod.Default.Util          (WidgetFileSettings, widgetFileNoReload,
                                    widgetFileReload)

-- runtime settings for application

data AppSettings = AppSettings
    { appStaticDir              :: String		--  Directory from which to serve static files.
    , appDatabaseConf           :: SqliteConf		-- Configuration settings for accessing the database.
    , appRoot                   :: Maybe Text		-- Base for all generated URLs
    , appHost                   :: HostPreference	 -- Host/interface the server should bind to.
    , appPort                   :: Int			-- Port to listen on
    , appIpFromHeader           :: Bool			-- Get the IP address from the header 

    , appDetailedRequestLogging :: Bool
    , appShouldLogAll           :: Bool			-- if all log messages be displayed
    , appReloadTemplates        :: Bool			-- to use the reloaded version of templates
    , appMutableStatic          :: Bool			-- to allow files in the static dir to change after compilation
    , appSkipCombining          :: Bool			 --  Perform no stylesheet/script combining
   
    , appCopyright              :: Text
    , appAnalytics              :: Maybe Text

    , appAuthDummyLogin         :: Bool
    }

instance FromJSON AppSettings where
    parseJSON = withObject "AppSettings" $ \o -> do
        let defaultDev =
#if DEVELOPMENT
                True
#else
                False
#endif
        appStaticDir              <- o .: "static-dir"		--giving default values for fields in appsettings datatype
        appDatabaseConf           <- o .: "database"
        appRoot                   <- o .:? "approot"
        appHost                   <- fromString <$> o .: "host"
        appPort                   <- o .: "port"
        appIpFromHeader           <- o .: "ip-from-header"

        appDetailedRequestLogging <- o .:? "detailed-logging" .!= defaultDev
        appShouldLogAll           <- o .:? "should-log-all"   .!= defaultDev
        appReloadTemplates        <- o .:? "reload-templates" .!= defaultDev
        appMutableStatic          <- o .:? "mutable-static"   .!= defaultDev
        appSkipCombining          <- o .:? "skip-combining"   .!= defaultDev

        appCopyright              <- o .:  "copyright"
        appAnalytics              <- o .:? "analytics"

        appAuthDummyLogin         <- o .:? "auth-dummy-login"      .!= defaultDev

        return AppSettings {..}

widgetFileSettings :: WidgetFileSettings  --Settings for 'widgetFile',
widgetFileSettings = def

combineSettings :: CombineSettings    --for static files
combineSettings = def

widgetFile :: String -> Q Exp         --for widget file
widgetFile = (if appReloadTemplates compileTimeAppSettings
                then widgetFileReload
                else widgetFileNoReload)
              widgetFileSettings

configSettingsYmlBS :: ByteString      -- raw bytes at compile time
configSettingsYmlBS = $(embedFile configSettingsYml)

-- @config/settings.yml@, parsed to a @Value@.
configSettingsYmlValue :: Value
configSettingsYmlValue = either Exception.throw id
                       $ decodeEither' configSettingsYmlBS

--version of @AppSettings@ that are parsed at compile time from @config/settings.yml@.
compileTimeAppSettings :: AppSettings
compileTimeAppSettings =
    case fromJSON $ applyEnvValue False mempty configSettingsYmlValue of
        Error e -> error e
        Success settings -> settings

combineStylesheets :: Name -> [Route Static] -> Q Exp  --to combine multiple CSS or JS files at compile time
combineStylesheets = combineStylesheets'
    (appSkipCombining compileTimeAppSettings)
    combineSettings

combineScripts :: Name -> [Route Static] -> Q Exp
combineScripts = combineScripts'
    (appSkipCombining compileTimeAppSettings)
    combineSettings
