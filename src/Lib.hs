{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}

module Lib
    ( startApp
    , initializeApp
    , AppEnv(..)
    ) where


import Network.Wai
import Network.Wai.Middleware.Cors
import Network.Wai.Handler.Warp
import Network.Wai.Logger       (withStdoutLogger)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Handler.WarpTLS   (runTLS, tlsSettings)
import Servant
import Servant.API.Experimental.Auth    (AuthProtect)
import Accounts.User                     (UserLogin(..))
import Servant.Utils.StaticFiles
import System.Environment (lookupEnv)
import DB (DB(..), initializeDB, getDB)
import Auth (Auth, Token(..), initializeTokens, authServerContext)
import Accounts (UserServer, LoginEndpoints, userServer)
import Accounts.User (UserLogin(..))
import Anki (AnkiServer, AnkiEndpoints, ankiServer )
import Configs as Configs (defaultConfig, Config(..), initializeJwt, Environment(..))

data AppEnv = AppEnv
    { configs :: Config
    , dbs     :: DB
    }

type API =  "anki" :> Auth :> Protected
        :<|> "login" :> Unprotected
        :<|> Raw

type Unprotected = LoginEndpoints
type Protected   = AnkiEndpoints

webRoot :: FilePath
webRoot = "web/build/"

startApp :: IO ()
startApp =
    withStdoutLogger $ \aplogger -> do
    appEnv <- initializeApp >> initializeApp
    let port     = serverPort appEnv
        logger   = serverLogs appEnv
        settings = setPort port $ setLogger aplogger defaultSettings
    putStrLn $ serverMessage appEnv
    case env appEnv of
        Production -> runTLS (tlsSettings "server.crt" "server.key") settings $ logger $ app appEnv
        _          -> runSettings settings $ logger $ app appEnv
    where serverPort    = port . configs
          serverLogs    = logger . configs
          env           = getEnv . configs
          serverMessage env = "Server Running on port: " ++ (show $ serverPort env)


initializeApp :: IO AppEnv
initializeApp = do
    dbs           <- initializeDB
    defaultConfig <- Configs.defaultConfig
    initializeJwt >> return (AppEnv defaultConfig dbs)

app :: AppEnv -> Application
app appEnv = appCors $ serveWithContext api (authServerContext appConfigs) $ server appEnv
    where appCors    = corsMW . configs $ appEnv
          appConfigs = configs appEnv

api :: Proxy API
api = Proxy

server :: AppEnv -> Server API
server appEnv = protected appDB :<|> unprotected appEnv :<|> serveDirectoryFileServer webRoot
    where appDB = dbs appEnv

protected :: DB -> Token -> AnkiServer
protected db t = ankiServer ankis t
    where ankis    = ankiDB db
          tokens   = tokenDB db

unprotected :: AppEnv -> UserServer
unprotected appEnv = userServer appConfig accounts tokens
  where db = dbs appEnv
        accounts  = accountDB db
        tokens    = tokenDB db
        appConfig = configs appEnv
