--module for the billing transactions

module Handler.Paybill where

import Import hiding (atomically)
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
import Text.Julius (RawJS (..))
import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Data.IORef
import System.IO.Unsafe


data FileForm = FileForm              --defining the elements of the form
    { personId :: Text
    , personRegion :: Int
    , personAmount :: Int
    }
    deriving Show

{-# NOLINE lock #-}
sem1 :: MVar ()                                 --declaring the MVar lock as global
sem1 = unsafePerformIO $ newMVar ()

sampleForm :: Form FileForm                                             --function for creating the form
sampleForm = renderBootstrap3 BootstrapBasicForm $ FileForm
    <$> areq textField "Bill type: " Nothing
    <*> areq intField "Region:  " Nothing
    <*> areq intField "Amount to be paid:  " Nothing 
    
        
getPaybillR :: Handler Html
getPaybillR  =  do
    (formWidget, formEnctype) <- generateFormPost sampleForm
    let submission = Nothing :: Maybe FileForm
        handlerName = "getPaybillR" :: Text

    defaultLayout $ do
         setTitle "Pay bill"
         $(widgetFile "paybill")                            --directing to the paybill.hamlet file
         
postPaybillR :: Handler Html
postPaybillR  = do
    ((result, formWidget), formEnctype) <- runFormPost sampleForm
    (first, user) <- requireAuthPair                         --getting the user id
    case result of
            FormSuccess res -> do
                 if userBalance user >= Just (personAmount res) && ((personRegion res) >=1 && (personRegion res)<=5) && (personAmount res)>0      --checking if details are valid
                  then do 
                     redirect $ CheckEqualR (personId res)(personRegion res)(personAmount res)    --if so directing to the checkequal handler
                  else do
                     defaultLayout $(widgetFile "billfail")              --else directing to the billfail.hamlet file
            _ -> do         
               defaultLayout $(widgetFile "billpaid")   
                        

getTransferBill :: Int -> Text -> Handler()  --function for transfer
getTransferBill amount id = do 
   xs <- takeMVar sem1                 --locking the mvar lock
   (first, user) <- requireAuthPair
   runDB $ updateWhere [UserIdent ==. id] [UserBalance +=. Just amount]      --updating balances in the user accounts
   runDB $ update first [ UserBalance -=. Just amount ]
   --updating values of the last 3 transactions
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
           runDB $ updateWhere [UserIdent ==. (userIdent user)][UserAmt3 =. Just (-amount), UserId3 =. Just id,UserCount=. 0] 
           runDB $ updateWhere [UserIdent ==. id][UserAmt3 =. Just amount, UserId3 =. Just (userIdent user),UserCount +=. 1]
   putMVar sem1 xs            --unlocking the mvar lock
   

getCheckEqualR :: Text -> Int -> Int -> Handler Html
getCheckEqualR id region amount = do
  let c = region 
  let s1 = show c
  let s2 = "E" ++ s1
  let s3 = "W" ++ s1
  let s4 = "G" ++ s1
  
  if id == "Electricity Bill"              --checking the input if it belongs to electricity bill or gas bill or water bill
     then do
       let t1 = pack (s2)
       getTransferBill (amount)(t1)
       redirect $ Suc_billR                --if details are valid go the succesful bill transfer handler
     else if id=="Water Bill"
       then do
         let t1 = pack (s3)
         getTransferBill (amount)(t1)
         redirect $ Suc_billR
       else if id=="Gas Bill"
          then do
             let t1 = pack (s4)
             getTransferBill (amount)(t1)
             redirect $ Suc_billR
          else do
              defaultLayout $(widgetFile "billfail")                    --if details are invalid the payment will fail
   




   

        
        



 
