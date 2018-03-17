--module for the home page
module Handler.Home where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
import Text.Julius (RawJS (..))

-- This is a handler function for the GET request method on the HomeR resource pattern. 

getHomeR :: Handler Html
getHomeR = do
    defaultLayout $ do
        setTitle "Netbanking app!"            
        $(widgetFile "homepage")              --directing to homepage.hamlet file

postHomeR :: Handler Html
postHomeR = do
    defaultLayout $ do
        setTitle "Netbanking app!"
        $(widgetFile "homepage")              --directing to homepage.hamlet file


