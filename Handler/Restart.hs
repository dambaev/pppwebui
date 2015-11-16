module Handler.Restart where

import Import
import System.Process
import Data.Text as T

postRestartR :: Handler Html
postRestartR = do
    app <- getYesod >>= return . appSettings
    liftIO $! callCommand $ T.unpack $ appRestartCmd app
    setMessage "перезапуск подключения"
    redirect StatusR

