module Handler.Settings where

import Import
import Util.CHAPFile
import Data.Text as T

getSettingsR :: Handler Html
getSettingsR = do
    app <- getYesod >>= return . appSettings
    manyidents <- liftIO $! getChapSecrets $ T.unpack $ appSecretFile app 
    let mident = case manyidents of
            [] -> Nothing
            (some:_) -> Just $! some
        menuW = $(widgetFile "menu")
    (widget, enctype) <- generateFormPost $ identForm mident
    defaultLayout $ do
        $(widgetFile "settings")

postSettingsR :: Handler Html
postSettingsR = do
    app <- getYesod >>= return . appSettings
    ((result, widget), enctype) <- runFormPost $ identForm Nothing
    case result of
        FormSuccess ident -> do
            liftIO $ saveChapSecrets (T.unpack $ appSecretFile app) [ident]
            setMessage "Параметры сохранены"
            redirect StatusR
        _ -> do
            setMessage "Введены некорректные данные, попробуйте еще раз"
            redirect SettingsR

identForm :: Maybe CHAPIdent-> Html -> MForm Handler (FormResult CHAPIdent, Widget)
identForm mident = renderDivs $ CHAPIdent
    <$> areq textField "Логин" (maybe Nothing (Just . ciClient) mident)
    <*> areq textField "Сервер" (maybe Nothing (Just . ciServer) mident)
    <*> areq textField "Пароль" (maybe Nothing (Just . ciSecret) mident)
    <*> pure Nothing

