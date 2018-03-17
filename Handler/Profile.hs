--module containing the password form

module Handler.Profile where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
import Text.Julius (RawJS (..))

data PasswordForm = PasswordForm            --declaring the elements of the form
    { pin :: Int
    }
    deriving Show


getProfileR :: Handler Html
getProfileR = do
    (formWidget, formEnctype) <- generateFormPost sampleForm
    let submission = Nothing :: Maybe PasswordForm
        handlerName = "getProfileR" :: Text
    
    defaultLayout $ do
         setTitle "Profile"
         $(widgetFile "takepassword")                                       --directs to the  takepassword hamlet file which contains the password form
 
 
postProfileR :: Handler Html
postProfileR  = do
    ((result, formWidget), formEnctype) <- runFormPost sampleForm
    (first, user) <- requireAuthPair                                  --getting userid
    case result of
            FormSuccess res -> do
                 if isNothing(userPassword user)                 --if userpassword in database is empty it means that it is a new user
                  then do 
                     redirect $ SetpasswordR (pin res)           --set your password
                  else do
                     redirect $ CheckpasswordR (pin res)          --else if entry is not empty check if the entered password matches with the value in the database
            _ -> do         
               defaultLayout $(widgetFile "error")   
        
        
getSetpasswordR :: Int -> Handler()                     --handler function for setting password
getSetpasswordR pin_num = do 
   (first, user) <- requireAuthPair
   runDB $ update first [ UserPassword =. Just pin_num ]                            --updating userpassword in the database
   redirect $ AllowtransR                                            --allowing transfer

  
sampleForm :: Form PasswordForm                                  --function for creating form
sampleForm = renderBootstrap3 BootstrapBasicForm $ PasswordForm
    <$> areq intField "Password: " Nothing
        
                
