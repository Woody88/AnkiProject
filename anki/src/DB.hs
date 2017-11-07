module DB (DB(..), initializeDB, getDB) where

import Accounts (AccountDB, initializeAccountsDB)
import Anki (AnkiDB, initializeAnkiDB)
import Data.Map (Map)
import qualified Data.Map.Lazy as Map
import qualified Control.Concurrent.STM    as T
import Data.Binary as B

data DB = DB { accountDB :: AccountDB
             , ankiDB    :: AnkiDB
             }

initializeDB :: IO DB
initializeDB = do
             putStrLn "Initializing Database..."
             accountDB <- initializeAccountsDB
             ankiDB    <- initializeAnkiDB "anki_list.csv"
             let db = setDB (accountDB, ankiDB)
             return db
  where setDB (acc, anki) = DB { accountDB = acc
                               , ankiDB    = anki
                               }

getDB :: String -> Map String a -> Maybe a
getDB = Map.lookup
