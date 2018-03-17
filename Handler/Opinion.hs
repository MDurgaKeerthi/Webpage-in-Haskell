module Handler.Opinion where

import Import

postOpinionR :: Handler Value
postOpinionR = do
    -- ToJSON and FromJSON instances are derived in the config/models file
    opinion <- (requireJsonBody :: Handler Opinion)

    maybeCurrentUserId <- maybeAuthId
    let opinion' = opinion { opinionUserId = maybeCurrentUserId }

    insertedOpinion <- runDB $ insertEntity opinion'
    returnJson insertedOpinion
