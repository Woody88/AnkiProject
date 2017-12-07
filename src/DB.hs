
module DB (DB(..), initializeDB, getDB) where

import Accounts (AccountDB, initializeAccountsDB)
import Anki (AnkiDB, initializeAnkiDB)
import Data.Map (Map)
import qualified Data.Map.Lazy as Map
import qualified Control.Concurrent.STM    as T
import Data.Binary as B
import Auth (Tokens, initializeTokens)

data DB = DB { accountDB :: AccountDB
             , ankiDB    :: AnkiDB
             , tokenDB   :: Tokens
             } 


initializeDB :: IO DB
initializeDB = do
             putStrLn "Initializing Database..."
             accountDB <- initializeAccountsDB
             ankiDB    <- initializeAnkiDB "anki_list.csv"
             tokenDB   <- initializeTokens
             let db = setDB (accountDB, ankiDB, tokenDB)
             return db
  where setDB (acc, anki, tks) = DB { accountDB = acc
                                    , ankiDB    = anki
                                    , tokenDB   = tks
                                    }

getDB :: String -> Map String a -> Maybe a
getDB = Map.lookup
