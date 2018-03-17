--module for the feedback form
module Handler.Feedback where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
import Text.Julius (RawJS (..))

getFeedbackR :: Handler Html
getFeedbackR = do
   defaultLayout $ do
        let (opinionFormId, opinionTextareaId, opinionListId) = opinionIds    --calling the responseIds function
        aDomId <- newIdent
        setTitle "Feedback!"
        $(widgetFile "feedback")    --loading feedback.hamlet widget

postFeedbackR :: Handler Html
postFeedbackR = do
    defaultLayout $ do
        let (opinionFormId, opinionTextareaId, opinionListId) = opinionIds
        aDomId <- newIdent
        setTitle "Feedback!"
        $(widgetFile "feedback")            



opinionIds :: (Text, Text, Text)            --function for creating feedback
opinionIds = ("js-opinionForm", "js-createopinionTextarea", "js-opinionList")
