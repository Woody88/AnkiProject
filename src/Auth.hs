{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE TemplateHaskell       #-}

module Auth
  ( Auth
  , Token(..)
  , Tokens
  , authHandler
  , initializeTokens
  , authServerContext
  , issueToken) where

import Control.Monad.IO.Class         (liftIO)
import Control.Monad.Reader           (ask, local)
import GHC.Generics (Generic)
import Data.Aeson                          as A
import Data.Aeson.TH
import qualified Control.Concurrent.STM    as T
import qualified Data.Map.Lazy             as Map
import qualified Data.ByteString.Char8     as C
import qualified Data.ByteString.Lazy      as B
import qualified Data.List                 as L
import Data.Text                        (pack, unpack)
import Data.Text.Encoding               (decodeUtf8)
import qualified Data.Text.Lazy.Encoding   as T
import qualified Data.Text.Lazy            as TL
import Data.List                        (stripPrefix)
import Network.Wai                      (Request, requestHeaders)
import Servant                          (throwError)
import Servant.Server                   (Handler, ServantErr(..),Context ((:.), EmptyContext),
                                         err401, err403, err404, errBody, Server, ServantErr,)
import Servant.API.Experimental.Auth    (AuthProtect)
import Servant.Server.Experimental.Auth (AuthHandler, AuthServerData,
                                         mkAuthHandler)
import System.Environment               (lookupEnv)
import Accounts.User                    (UserLogin(..))
import Jose.Jwe
import Jose.Jwa
import Jose.Jwk (Jwk(..), generateRsaKeyPair, generateSymmetricKey, KeyUse(Enc), KeyId)
import Jose.Jwt
import App
import Configs

type Auth = AuthProtect "jwt-auth"


type RsaPair = (Jwk, Jwk)
type TokenStore = Map.Map KeyId RsaPair
type Tokens = T.TMVar TokenStore
type JWT = (KeyId, RsaPair)


newtype Token   = Token { token :: String } deriving (Eq, Read, Show, Generic)

$(deriveJSON defaultOptions ''Token)

type instance AuthServerData (AuthProtect "jwt-auth") = Token

initializeTokens :: IO (Tokens)
initializeTokens = T.atomically $ T.newTMVar Map.empty

tokens :: Tokens -> Handler [JWT]
tokens tksThread = do
  tks <- liftIO $ (T.atomically . T.readTMVar) tksThread
  return $ Map.toList tks

createToken :: Tokens -> String -> Handler JWT
createToken tksThread usr = do
  tokens <- liftIO $ (T.atomically . T.takeTMVar) tksThread
  rsa   <- liftIO $ (generateRsaKeyPair 256 key Enc Nothing)
  let tokensToUpdate = Map.insert key rsa tokens
  updatedStorage <- liftIO $ (T.atomically . (T.putTMVar tksThread)) tokensToUpdate
  return $ (key, rsa)
  where key = (KeyId $ pack usr)

getToken :: Tokens -> String -> Handler (Maybe RsaPair)
getToken tksThread usr = do
  tks <- liftIO $ (T.atomically . T.readTMVar) tksThread
  return $ Map.lookup (KeyId $ pack usr) tks

issueToken :: Jwk -> Tokens -> String -> AppH Token
issueToken publicKey tks email = do
  Right (Jwt jwt) <- liftIO $ jwkEncode RSA_OAEP A128GCM publicKey (Claims "my claim")
  return $ (Token $ C.unpack jwt)

authHandler :: Config -> AuthHandler Request Token
authHandler cfg = mkAuthHandler (\r -> returnH cfg $ authHandler' r)

authHandler' :: Request -> AppH Token
authHandler' req = do
  config <- ask
  let (Just privateKey) = privKey config
      handler req' = case lookup "Authorization" (requestHeaders req) of
       Nothing -> throwError (err401 { errBody = "Missing auth header" })
       Just authKey ->
        case stripBearer $ C.unpack authKey of
          Nothing -> throwError (err401 { errBody = "Header Malformatted" })
          Just key -> verifyToken privateKey (C.pack key)
  handler req
  where stripBearer = stripPrefix "Bearer "


verifyToken :: Jwk -> C.ByteString -> AppH Token
verifyToken privateKey jwt = do
  token <- liftIO $ jwkDecode privateKey jwt
  let verified = case token of
                   Right (Jwe (hdr, claims)) -> return $ (Token (C.unpack jwt))
                   _                         -> throwError (err403 { errBody = "Invalid Token" })
  verified

authServerContext :: Config -> Context (AuthHandler Request Token ': '[])
authServerContext cfg = (authHandler cfg) :. EmptyContext
