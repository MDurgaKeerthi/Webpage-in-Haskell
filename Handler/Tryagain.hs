-- module for tryagain in case of failures
module Handler.Tryagain where

import Import

getTryagainR :: Handler Html
getTryagainR = do
    defaultLayout $ do
      setTitle "Try again"
      $(widgetFile "tryagain")              --directing to tryagain.hamlet file  
