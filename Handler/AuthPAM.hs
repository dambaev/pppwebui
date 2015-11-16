module Handler.AuthPAM where


import Yesod
import Yesod.Auth

import Prelude
import Data.Text as T
import System.Posix.PAM as PAM

authPAMGetLoginR = getLoginR
authPAMDispatch service "POST" ["login"] = checkLogin service >>= sendResponse
authPAMDispatch _ _ _ = notFound

postLoginR:: AuthRoute
postLoginR = PluginR "pam" ["login"]

getLoginR:: (Route Auth-> Route master ) -> WidgetT master IO ()
getLoginR tm = do
    [whamlet|
    <form method=POST action=@{tm postLoginR}>
        <table>
            <tr>
                <td> Имя пользователя:
                <td> <input type="text" name="login"  required>
            <tr>
                <td> Пароль:
                <td> <input type="password" name="password" required>
            <tr>
                <td>
                    <button>
                        Войти
|]


checkLogin:: (YesodAuth site) => Text-> HandlerT Auth (HandlerT site IO) Html
checkLogin service = do
    clearSession
    mlogin <- lookupPostParam "login"
    mpassword <- lookupPostParam "password"
    case (mlogin, mpassword) of
        (Nothing, _) -> badMethod
        ( _, Nothing) -> badMethod
        (Just login, Just password) -> do
            eret <- liftIO $ PAM.authenticate (T.unpack service) (T.unpack login) (T.unpack password)
            case eret of
                Left _ -> notFound
                Right _ -> do
                    lift $ setCreds True $ Creds "pam" login []
                    notFound 

    
