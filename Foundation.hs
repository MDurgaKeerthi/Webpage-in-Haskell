module Foundation where		--foundation for webpage : default layout, login, authentication, authorisation

{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Import.NoFoundation
import Database.Persist.Sql (ConnectionPool, runSqlPool)
import Text.Hamlet          (hamletFile)
import Text.Jasmine         (minifym)
import Yesod.Auth.Dummy
import Yesod.Default.Util   (addStaticContentExternal)
import Yesod.Core.Types     (Logger)
import qualified Yesod.Core.Unsafe as Unsafe
import qualified Data.CaseInsensitive as CI
import qualified Data.Text.Encoding as TE

data App = App
    { appSettings    :: AppSettings		--datatype app
    , appStatic      :: Static 
    , appConnPool    :: ConnectionPool
    , appHttpManager :: Manager
    , appLogger      :: Logger
    }

data MenuItem = MenuItem                --to display menu on navbar 
    { menuItemLabel :: Text
    , menuItemRoute :: Route App
    , menuItemAccessCallback :: Bool
    }

data MenuTypes
    = NavbarLeft MenuItem
    | NavbarRight MenuItem


mkYesodData "App" $(parseRoutesFile "config/routes")   --giving routes in external file

type Form x = Html -> MForm (HandlerT App IO) (FormResult x, Widget)   --synonym for forms

-- what is to be started as app
instance Yesod App where
    approot = ApprootRequest $ \app req ->
        case appRoot $ appSettings app of
            Nothing -> getApprootText guessApproot app req
            Just root -> root

   
    yesodMiddleware = defaultYesodMiddleware

--setting default layout for all the pages	
    defaultLayout widget = do
        master <- getYesod
        mmsg <- getMessage

        muser <- maybeAuthPair
        mcurrentRoute <- getCurrentRoute

        (title, parents) <- breadcrumbs

        let menuItems =
                [ NavbarLeft $ MenuItem
                    { menuItemLabel = "Home"
                    , menuItemRoute = HomeR
                    , menuItemAccessCallback = True
                    }
                , NavbarRight $ MenuItem
                    { menuItemLabel = "Feedback"
                    , menuItemRoute = FeedbackR
                    , menuItemAccessCallback = True
                    } 
                 , NavbarRight $ MenuItem
                    { menuItemLabel = "Instructions"
                    , menuItemRoute = InstructionsR
                    , menuItemAccessCallback = True
                    } 
                , NavbarRight $ MenuItem
                    { menuItemLabel = "Contact us"
                    , menuItemRoute = ContactR
                    , menuItemAccessCallback = True
                    }     
                , NavbarRight $ MenuItem
                    { menuItemLabel = "Login"
                    , menuItemRoute = AuthR LoginR
                    , menuItemAccessCallback = isNothing muser
                    }
                , NavbarRight $ MenuItem
                    { menuItemLabel = "Logout"
                    , menuItemRoute = AuthR LogoutR
                    , menuItemAccessCallback = isJust muser
                    }
                ]

        let navbarLeftMenuItems = [x | NavbarLeft x <- menuItems]
        let navbarRightMenuItems = [x | NavbarRight x <- menuItems]

        let navbarLeftFilteredMenuItems = [x | x <- navbarLeftMenuItems, menuItemAccessCallback x]
        let navbarRightFilteredMenuItems = [x | x <- navbarRightMenuItems, menuItemAccessCallback x]

        pc <- widgetToPageContent $ do
            addStylesheet $ StaticR css_bootstrap_css
            $(widgetFile "default-layout")
        withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

    --authorizing different pages
    authRoute _ = Just $ AuthR LoginR

    isAuthorized (AuthR _) _ = return Authorized
    isAuthorized OpinionR _ = return Authorized
    isAuthorized HomeR _ = return Authorized
    isAuthorized (StaticR _) _ = return Authorized
    isAuthorized _ _ = return Authorized

    --this for messages
    shouldLog app _source level = 
        appShouldLogAll (appSettings app)
            || level == LevelWarn
            || level == LevelError

    makeLogger = return . appLogger
    defaultMessageWidget title body = $(widgetFile "default-message-widget")

-- Define breadcrumbs - gives root for different page tittles.
instance YesodBreadcrumbs App where
  breadcrumb HomeR = return ("Home", Nothing)
  breadcrumb (AuthR _) = return ("Login", Just HomeR)
  breadcrumb FeedbackR = return ("Feedback", Just HomeR)
  breadcrumb ContactR = return ("Contact us", Just HomeR)
  breadcrumb InstructionsR = return ("Instructions", Just HomeR)
  breadcrumb  _ = return ("home", Nothing)

-- to run database actions.
instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB action = do
        master <- getYesod
        runSqlPool action $ appConnPool master
instance YesodPersistRunner App where
    getDBRunner = defaultGetDBRunner appConnPool

instance YesodAuth App where			--settings for login
    type AuthId App = UserId

    loginDest _ = ProfileR
    logoutDest _ = HomeR

    authenticate creds = runDB $ do
        x <- getBy $ UniqueUser $ credsIdent creds
	let v = 0 :: Int
        case x of
            Just (Entity uid _) -> return $ Authenticated uid        --inserting new user
            Nothing -> Authenticated <$> insert User
                { userIdent = credsIdent creds
                , userPassword = Nothing
                , userBalance = Just 10
                , userName = Just "user"
                , userAge = Just 18
                , userAmt1 = Just 0
                , userAmt2 = Just 0
                , userAmt3 = Just 0
                , userId1 = Just "No transaction"
                , userId2 = Just "No transaction"
                , userId3 = Just "No transaction"
                , userCount = v
                }
    
    
       
    authPlugins app = [ ] ++ extraAuthPlugins
      where extraAuthPlugins = [authDummy | appAuthDummyLogin $ appSettings app]   --login plugin
    authHttpManager = getHttpManager

-- this function to determine if a user is logged in.
isAuthenticated :: Handler AuthResult
isAuthenticated = do
    muid <- maybeAuthId
    return $ case muid of
        Nothing -> Unauthorized "You must login to access this page"
        Just _ -> Authorized

instance YesodAuthPersist App

-- This is for forms. 
instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

instance HasHttpManager App where
    getHttpManager = appHttpManager

unsafeHandler :: App -> Handler a -> IO a
unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger

