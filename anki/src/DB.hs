module DB (DB, initializeDB, getDB) where

import Accounts (AccountDB, initializeAccountsDB)
import Data.Map (Map)
import qualified Data.Map.Lazy as Map

type Database = AccountDB

type DB = Map String Database

initializeDB :: IO (DB)
initializeDB = do
  accounts <- putStrLn "Initializing Database..." >> initializeAccountsDB -- Set Memory DB for Accounts, later on will create a config file.
  return $ Map.insert "accountDB" accounts db
  where db = Map.empty

getDB :: String -> DB -> Maybe Database
getDB str dbs =
  Map.lookup str dbs
