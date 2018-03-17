-- module to check password
module Handler.Checkpassword where

import Import


getCheckpasswordR :: Int -> Handler()
getCheckpasswordR pin_num = do 
   (first, user) <- requireAuthPair  --gets user id
   if userPassword user == Just pin_num          --checking if the pin matches with the password in the database
      then do
         redirect $ AllowtransR   --allow transfer
      else do    
         redirect $ TryagainR     --else tryagain
