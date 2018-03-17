--module for the contact us page
module Handler.Contact where

import Import

getContactR :: Handler Html
getContactR = do
    defaultLayout $ do
      setTitle "Contact"
      $(widgetFile "contact")  --directing to the hamlet file contact.hamlet 
