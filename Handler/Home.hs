module Handler.Home where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3,
                              withSmallInput)

import Util.CHAPFile
                              
-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
{- getHomeR :: Handler Html
 getHomeR = do
    idents <- liftIO $! getChapSecrets "Util/orig.txt"
    defaultLayout $ do
        aDomId <- newIdent
        setTitle "Welcome To Yesod!"
        [whamlet|
<table>
    $forall ident <- idents
        <li> #{ciServer ident} #{length $ ciServer ident } #{ciClient ident} #{ciSecret ident}
|]
-}

getHomeR :: Handler Html
getHomeR = do
    defaultLayout $ do
        setTitle "PPP secrets editor"
        $(widgetFile "homepage")


postHomeR:: Handler Html
postHomeR = undefined

