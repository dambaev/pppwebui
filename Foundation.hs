module Foundation where

import Import.NoFoundation
import Text.Hamlet                 (hamletFile)
import Text.Jasmine                (minifym)
import Yesod.Core.Types            (Logger)
import Yesod.Default.Util          (addStaticContentExternal)
import qualified Yesod.Core.Unsafe as Unsafe
import Yesod.Auth
import Util.AAA
import Handler.AuthPAM
import Data.List as DL (any)
import System.Posix.Syslog as SPS
import Data.Text as T
import Data.Text.Encoding as E
import Network.Wai (remoteHost)

-- | The foundation datatype for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
    { appSettings    :: AppSettings
    , appStatic      :: Static -- ^ Settings for static file serving.
    , appHttpManager :: Manager
    , appLogger      :: Logger
    }

instance HasHttpManager App where
    getHttpManager = appHttpManager

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/routing-and-handlers
--
-- Note that this is really half the story; in Application.hs, mkYesodDispatch
-- generates the rest of the code. Please see the linked documentation for an
-- explanation for this split.
--
-- This function also generates the following type synonyms:
-- type Handler = HandlerT App IO
-- type Widget = WidgetT App IO ()
mkYesodData "App" $(parseRoutesFile "config/routes")

-- | A convenient synonym for creating forms.
type Form x = Html -> MForm (HandlerT App IO) (FormResult x, Widget)

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
    -- Controls the base of generated URLs. For more information on modifying,
    -- see: https://github.com/yesodweb/yesod/wiki/Overriding-approot
    approot = ApprootMaster $ appRoot . appSettings

    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
    makeSessionBackend _ = Just <$> defaultClientSessionBackend
        120    -- timeout in minutes
        "config/client_session_key.aes"

    defaultLayout widget = do
        master <- getYesod
        mmsg <- getMessage

        -- We break up the default layout into two components:
        -- default-layout is the contents of the body tag, and
        -- default-layout-wrapper is the entire page. Since the final
        -- value passed to hamletToRepHtml cannot be a widget, this allows
        -- you to use normal widget features in default-layout.

        pc <- widgetToPageContent $ do
            addStylesheet $ StaticR css_bootstrap_css
            $(widgetFile "default-layout")
        withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

    -- Routes not requiring authenitcation.
    isAuthorized FaviconR _ = return Authorized
    isAuthorized RobotsR _ = return Authorized
    isAuthorized (AuthR _) _ = return Authorized
    isAuthorized ExitR _ = return Authorized
    -- Default to Authorized for now.
    isAuthorized route method = do
        muserid <- maybeAuthId
        case muserid of
            Nothing-> do
                liftIO $ print route
                return $ AuthenticationRequired
            Just userid -> do
                app <- getYesod >>= return . appSettings
                egroup <- liftIO $ getUserGroups userid 
                case egroup of
                    Left _ -> return $ Unauthorized "Не удалось получить список Ваших прав"
                    Right groups -> checkAuthorization route method userid app groups

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent ext mime content = do
        master <- getYesod
        let staticDir = appStaticDir $ appSettings master
        addStaticContentExternal
            minifym
            genFileName
            staticDir
            (StaticR . flip StaticRoute [])
            ext
            mime
            content
      where
        -- Generate a unique filename based on the content itself
        genFileName lbs = "autogen-" ++ base64md5 lbs

    -- What messages should be logged. The following includes all messages when
    -- in development, and warnings and errors in production.
    shouldLog app _source level =
        appShouldLogAll (appSettings app)
            || level == LevelWarn
            || level == LevelError

    makeLogger = return . appLogger
    authRoute _ = Just $ AuthR LoginR

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

unsafeHandler :: App -> Handler a -> IO a
unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger

-- Note: Some functionality previously present in the scaffolding has been
-- moved to documentation in the Wiki. Following are some hopefully helpful
-- links:
--
-- https://github.com/yesodweb/yesod/wiki/Sending-email
-- https://github.com/yesodweb/yesod/wiki/Serve-static-files-from-a-separate-domain
-- https://github.com/yesodweb/yesod/wiki/i18n-messages-in-the-scaffolding

instance YesodAuth App where
    type AuthId App = Text


    getAuthId = return . Just . credsIdent

    loginDest _ = StatusR
    logoutDest _ = StatusR


    authPlugins _ = [authPAM]
    authHttpManager = appHttpManager

    maybeAuthId = lookupSession "_ID"


authPAM:: AuthPlugin App
authPAM = AuthPlugin "pam" dispatch authPAMGetLoginR
    where
    dispatch method urls = do
        app <- lift $ getYesod >>= return . appSettings
        authPAMDispatch (appPAMService app) method urls
-- check connection status
checkAuthorization StatusR _ user app groups | DL.any (==(appACLStatus app)) groups = do
    syslogMsg $ user `T.append` ": read status"
    return Authorized
-- restart connection
checkAuthorization RestartR True user app groups | DL.any (==(appACLRestart app)) groups = do
    syslogMsg $ user `T.append` ": restart connection"
    return Authorized
-- view settings
checkAuthorization SettingsR False user app groups | DL.any (==(appACLSettingsR app)) groups = do
    syslogMsg $ user `T.append` ": read settings"
    return Authorized
-- change settings
checkAuthorization SettingsR True user app groups | DL.any (==(appACLSettingsW app)) groups = do
    syslogMsg $ user `T.append` ": set settings"
    return Authorized
checkAuthorization route method user _ _ = do
    syslogMsg $ user `T.append` ": access denied to " `T.append` (T.pack $ show route) `T.append` ", write: " 
        `T.append` (T.pack $ show method)
    return $ Unauthorized "У Вас недостаточно прав. Обратитесь к администратору"


syslogMsg:: Text-> Handler ()
syslogMsg msg = do
    app <- getYesod >>= return . appSettings
    ip <- getClientIP
    liftIO $ SPS.withSyslog (T.unpack $ appSyslogID app) [PID] AUTH [Notice] 
        $ SPS.syslog Notice (T.unpack $ "[" `T.append` ip `T.append` "]: " `T.append` msg)
    return ()

getClientIP:: Handler Text
getClientIP = do
    mrealip <- lookupHeader "X-Real-IP" >>= \x-> case x of
        Nothing -> return "nothing"
        Just ip -> return $ E.decodeUtf8 ip
    mforwarderFor <- lookupHeader "X-Forwarder-For" >>= \x-> case x of
        Nothing -> return "nothing"
        Just ip -> return $ E.decodeUtf8 ip
    host <- remoteHost <$> waiRequest
    return $ (mrealip) `T.append` "->" `T.append` mforwarderFor `T.append` "->" 
        `T.append` (T.pack$ show host)
