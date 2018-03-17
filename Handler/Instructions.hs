--module for the instructions page
module Handler.Instructions where

import Import

getInstructionsR :: Handler Html
getInstructionsR = do
    defaultLayout $ do
       setTitle "Instructions"
       $(widgetFile "instructions")                    --directs to the instructions.hamlet file
