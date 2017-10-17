{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}

module Accounts where

-- , createUser, getUser, loginUser,
import Accounts.User (User, UserLogin, Accounts, users, createUser, getUser, initializeAccounts)
import Servant
import Control.Monad.IO.Class
import Anki

type UserServer = UserApi
type AccountDB = Accounts

type UserApi =  "users" :> Get '[JSON] [User]
           :<|> "user"  :> ReqBody '[JSON] User :> Post '[JSON] User
           :<|> "user"  :> Capture "x" Int :> Get '[JSON] (Maybe User)
           -- :<|> "login" :> ReqBody '[JSON] UserLogin :> Post '[JSON] (Maybe User)

userServer :: AccountDB -> Server UserServer
userServer accDB =  userList
         :<|> newUser
         :<|> user
         -- :<|> login
  where userList  = liftIO $ users accDB
        newUser u = liftIO $ createUser accDB u
        user u = liftIO $ getUser accDB u
        -- login (UserLogin e p) = return $ loginUser accDB e p

initializeAccountsDB :: IO AccountDB
initializeAccountsDB =  initializeAccounts
