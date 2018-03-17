--module for successful bill
module Handler.Suc_bill where

import Import

getSuc_billR :: Handler Html
getSuc_billR  = do 
   (_, user) <- requireAuthPair             --getting userid
   defaultLayout $ do
         setTitle "Bill paid!"
         $(widgetFile "billpaid")           --directing to the billpaid hamlet file
