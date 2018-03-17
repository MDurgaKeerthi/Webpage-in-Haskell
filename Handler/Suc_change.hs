-- module for successful changes in details
module Handler.Suc_change where

import Import

getSuc_changeR :: Handler Html
getSuc_changeR  = do 
   (_, user) <- requireAuthPair                     --getting userid
   defaultLayout $ do
         setTitle "Profile"
         $(widgetFile "suc_change")                 --directing to suc_change hamlet file
