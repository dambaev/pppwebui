module Handler.Exit where

import Import

getExitR :: Handler Html
getExitR = do
    clearSession
    defaultLayout $! do
        $(widgetFile "exit")


