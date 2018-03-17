-- module for allowing transaction after successful login
module Handler.Allowtrans where

import Import

getAllowtransR :: Handler Html
getAllowtransR = do
    (first, user) <- requireAuthPair         --getting user id
    defaultLayout $ do
       setTitle "successful transaction"
       $(widgetFile "allowtrans")              --directing to allowtrans.hamlet file            

       case (userName user) of
         Just user -> [whamlet| <div .ui.container>
                              <li .list-group-item><h4>Name: #{user}
                    |]
       case (userAge user) of
         Just user -> [whamlet| <div .ui.container>
                              <li .list-group-item><h4>Age: #{user}
                    |] 	
       case (userBalance user) of
         Just user -> [whamlet|<div .ui.container>
                             <li .list-group-item><h4>Your Balance: #{user}
                    |]
       --printing the last three transactions of the user
       case (userId1 user) of
         Just user -> [whamlet|<div .ui.container>
                              <h3><u> Last three Transactions</u>
                              <h4>Source: #{user}
                    |]


       case (userAmt1 user) of
         Just user -> [whamlet| <div .ui.container>
                              <h4>Transferred Amount: #{user}
                    |]
       

       case (userId2 user) of
         Just user -> [whamlet|  <div .ui.container>
                              <h4>Source: #{user}
                    |]


       case (userAmt2 user) of
         Just user -> [whamlet|<div .ui.container>
                             <h4>Transferred Amount: #{user}
                    |]

       case (userId3 user) of
         Just user -> [whamlet|<div .ui.container>
                              <h4>Source: #{user}
                    |]


       case (userAmt3 user) of
         Just user -> [whamlet|<div .ui.container>
                              <h4>Transferred Amount: #{user}
                    |]
                    
      -- printing the current balance of the user
       
       
