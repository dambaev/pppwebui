module Handler.Status where

import Import

import Yesod.Auth
import Data.ByteString as BS
import Data.ByteString.Char8 as C8
import System.Process
import System.Exit
import Data.Text as T
import Data.Text.Encoding as E
import Handler.AuthPAM


getStatusR :: Handler Html
getStatusR = do
    userid <- requireAuthId
    app <- getYesod >>= return . appSettings
    mstatus <- liftIO $! getConnectionStatus $ appStatusCmd app
    
    let menuW = $(widgetFile "menu")
    defaultLayout $! do
        $(widgetFile "status")

getConnectionStatus:: Text-> IO (Maybe [Text])
getConnectionStatus cmd = do
    (code, out, err) <- readProcessWithExitCode (T.unpack cmd) [] "" 
    case code of
        ExitSuccess -> do
            case Import.length out of
                _ -> return $! Just $! T.lines $! T.pack out
        _ -> return Nothing


