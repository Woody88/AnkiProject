#!/usr/bin/env stack
-- stack script --resolver lts-9.6
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Accounts.User (User(..), UserLogin(..), Accounts, initializeAccounts, users, createUser, getUser) where

import Prelude
import Data.Csv
import Data.Aeson
import Data.Aeson.TH
import Control.Monad
import Data.List (find)
import GHC.Generics (Generic)
import Servant.Server                   (Handler, err401, err403, err404, errBody)
import Servant.Elm  (ElmType)
import Data.Map (Map)
import qualified Data.Map.Lazy as Map
import qualified Control.Concurrent.STM    as T

data User = User
  { userId        :: Int
  , userFirstName :: String
  , userLastName  :: String
  , userPassword  :: String
  } deriving (Eq, Show, Generic)

data UserLogin = UserLogin
    { email    :: String
    , password :: String
    } deriving (Eq, Read, Show, Generic)

instance ElmType User


$(deriveJSON defaultOptions ''User)
$(deriveJSON defaultOptions ''UserLogin)


type Storage = Map Int User
type Accounts = T.TMVar Storage


initializeAccounts :: IO (Accounts)
initializeAccounts = T.atomically $ T.newTMVar $ Map.insert (1::Int) adminUser storage
  where adminUser = User 1 "Woodson" "Delhia" "1234"
        storage = Map.empty

users :: Accounts -> IO [User]
users accThread = do
    accs <- (T.atomically . T.readTMVar) accThread
    return  (Map.elems accs)

createUser :: Accounts -> User -> IO User
createUser accThread user = do
    storage <- (T.atomically . T.takeTMVar) accThread
    let (updatedUser, updatedAccs) = addUser storage user
    updatedStorage <- (T.atomically . (T.putTMVar accThread)) updatedAccs
    return updatedUser

addUser :: Storage -> User -> (User, Storage)
addUser storage user =  (updatedUser, Map.insert newId updatedUser storage)
    where totalUsers = Map.size storage
          newId = totalUsers + 1
          updatedUser = user { userId = newId }

getUser :: Accounts -> Int -> IO (Maybe User)
getUser accThread id_ = do
    storage <- (T.atomically . T.readTMVar) accThread
    return $ Map.lookup id_ storage
