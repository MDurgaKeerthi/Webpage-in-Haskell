-- module for successful transaction
module Handler.Suc_trans where

import Import


getSuc_transR :: Handler Html
getSuc_transR  = do 
   (_, user) <- requireAuthPair            --getting userid
   defaultLayout $ do
         setTitle "Profile"
         $(widgetFile "transfer")            -- directing to transfer.hamlet file
