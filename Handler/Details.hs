module Handler.Details where
-- module for updating user details
import Import 
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
import Text.Julius (RawJS (..))



data DetailsForm = DetailsForm            --elements of the form
    { newPassword :: Int
    , newName   :: Text
    , newAge :: Int
    }
    deriving Show

sampleForm :: Form DetailsForm
sampleForm = renderBootstrap3 BootstrapBasicForm $ DetailsForm
    <$> areq intField "New Password: " Nothing                     --new password
    <*> areq textField "Your Name:  " Nothing                      --name
    <*> areq intField "Your Age:  " Nothing                        --age
    
        
getDetailsR :: Handler Html
getDetailsR  =  do
    (formWidget, formEnctype) <- generateFormPost sampleForm   --generating form
    let submission = Nothing :: Maybe DetailsForm
        handlerName = "getDetailsR" :: Text

    maybeCurrentUserId <- maybeAuthId            --getting user id

    defaultLayout $ do
         setTitle "Profile"
         $(widgetFile "details")                  --goes to details.hamlet file


getChangeR :: Int -> Text -> Int -> Handler()         --handler function to change details
getChangeR pswd name age = do 
   (_, user) <- requireAuthPair
   runDB $ updateWhere [UserIdent ==. (userIdent user)] [UserPassword =. Just pswd, UserName =. Just name, UserAge =. Just age]   --updating details in the database
   redirect $ Suc_changeR
  


   
postDetailsR :: Handler Html
postDetailsR  = do
    ((result, formWidget), formEnctype) <- runFormPost sampleForm
    (first, user) <- requireAuthPair
    case result of
      FormSuccess res -> do
         redirect $ ChangeR (newPassword res)(newName res)(newAge res)            --after taking input from form directing to the change handler
      _ -> do         
         defaultLayout $(widgetFile "error")   
        
        



 
