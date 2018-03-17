--moduel for making transactions between user accounts
module Handler.Transaction where
import  Import 
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
import Text.Julius (RawJS (..))
import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Data.IORef
import System.IO.Unsafe
import Data.List

data InfoForm = InfoForm                 --declaring the elements of the form
    { personName :: Text
    , personId   :: Text
    , personAmount :: Int
    }
    deriving Show

{-# NOLINE lock #-}
sem1 :: MVar ()                                               --defining mvar lock as global variable
sem1 = unsafePerformIO $ newMVar ()

sampleForm :: Form InfoForm                                             --function for creating form
sampleForm = renderBootstrap3 BootstrapBasicForm $ InfoForm
    <$> areq textField "Sender's Name: " Nothing
    <*> areq textField "Reciever's name:  " Nothing 
    <*> areq intField "Amount to transfer:  " Nothing 
 
        
getTransactionR :: Handler Html                                                                
getTransactionR  =  do
    (formWidget, formEnctype) <- generateFormPost sampleForm
    let submission = Nothing :: Maybe InfoForm
        handlerName = "getTransactionR" :: Text
    defaultLayout $ do
         setTitle "Transactions"
         $(widgetFile "transaction")                                    --directing to transaction.hamlet file


getTransfer ::  Int -> Text -> Handler()                     --function for transfer of amount  
getTransfer  amount id = do 
   xs <- takeMVar sem1                                 --locking the mvar lock
   (first, user) <- requireAuthPair
   id1 <- runDB $ getBy404 $ UniqueUser id                       --checking if the given user exists in the database
   runDB $ update first [ UserBalance -=. Just amount ]                               --updating balances in the user accounts
   runDB $ updateWhere [UserIdent ==. id] [UserBalance +=. Just amount]
   -- updating the last three transactions in the database
   if userCount user ==3
     then do 
          runDB $ updateWhere [UserIdent ==. (userIdent user)][UserCount=. 0] 
     else
          runDB $ updateWhere [UserIdent ==. (userIdent user)][UserCount+=. 0]   
   if userCount user == 0
     then do
        runDB $ updateWhere [UserIdent ==. (userIdent user)][UserAmt1 =. Just (-amount), UserId1 =. Just id,UserCount +=. 1]
        runDB $ updateWhere [UserIdent ==. id][UserAmt1 =. Just amount, UserId1 =. Just (userIdent user),UserCount+=.1]
     else if userCount user == 1
         then do
          runDB $ updateWhere [UserIdent ==. (userIdent user)][UserAmt2 =. Just (-amount), UserId2 =. Just id,UserCount +=. 1]
          runDB $ updateWhere [UserIdent ==. id][UserAmt2 =. Just amount, UserId2 =. Just (userIdent user),UserCount +=. 1]
         else do
           runDB $ updateWhere [UserIdent ==. (userIdent user)][UserAmt3 =. Just (-amount), UserId3 =. Just id,UserCount+=. 1] 
           runDB $ updateWhere [UserIdent ==. id][UserAmt3 =. Just amount, UserId3 =. Just (userIdent user),UserCount +=. 1]

   putMVar sem1 xs                 --unlocking the lock
  


   
postTransactionR :: Handler Html
postTransactionR  = do
    ((result, formWidget), formEnctype) <- runFormPost sampleForm
    (first, user) <- requireAuthPair               --getting userid
    case result of
            FormSuccess res -> do
                 if userBalance user >= Just (personAmount res) && (personAmount res)>0 && (personId res)/=(userIdent user)   --checking if details are valid
                  then do 
                     getTransfer  (personAmount res)(personId res)         --calling the transfer function   
                     redirect $ Suc_transR                                  --redirecting to suc_trans handler
                  else do
                     defaultLayout $(widgetFile "transferfail")             --if details are invalid then the transfer will fail
            _ -> do         
               defaultLayout $(widgetFile "transfer")   
        
        



 
