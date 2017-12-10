{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Accounts where


import Control.Monad.Reader
import App
import Configs
import Accounts.User                      (User(..), UserLogin(..), Accounts, users, createUser, getUser, initializeAccounts)
import Servant
import Servant.Server                     (err401)
import Control.Monad.IO.Class             (liftIO)
import Anki
import GHC.Generics (Generic)
import Data.Aeson
import Data.Aeson.TH
import Auth                               (Token(..), Tokens, issueToken)
import           Jose.Jwk                             (Jwk(..))

type UserServer = Server LoginEndpoints

type AccountDB = Accounts

type LoginEndpoints =  ReqBody '[JSON] UserLogin
                       :> Post '[JSON] Token

-- type UserApi =  "users" :> Get '[JSON] [User]
--            :<|> "user"  :> ReqBody '[JSON] User :> Post '[JSON] User
--            :<|> "user"  :> Capture "id" Int :> Get '[JSON] (Maybe User)
           -- :<|> "login" :> ReqBody '[JSON] UserLogin :> Post '[JSON] (Maybe User)


userServer :: Config -> AccountDB -> Tokens -> UserServer
userServer cfg accDB  tokenDB = login'
    where login' u = returnH cfg $ login accDB tokenDB u

login :: AccountDB -> Tokens -> UserLogin -> AppH Token
login a t  u = do
  liftIO $ putStrLn (email u)
  verifyLogin a t u

verifyLogin :: Accounts -> Tokens -> UserLogin -> AppH Token
verifyLogin accDB tokenDB (UserLogin email password) = do
  configs <- ask
  let publicKey = pubKey configs
  if (email == "wdelhia" && password == "password")
  then issueToken publicKey tokenDB email
  else throwError (err401 { errBody = "Invalid User" })

initializeAccountsDB :: IO AccountDB
initializeAccountsDB =  initializeAccounts
