{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}

module Anki (AnkiDB, AnkiServer, initializeAnkiDB, ankiServer) where

import Data.Csv
import Servant
import Control.Monad.IO.Class
import Anki.AnkiCard (AnkiCards, AnkiCard(..), ankiCards, initializeAnkiCards)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V

type AnkiDB = AnkiCards
type AnkiServer = AnkiApi
type AnkiApi = "ankis" :> Get '[JSON] [AnkiCard]
             -- :<|> "user"  :> ReqBody '[JSON] User :> Post '[JSON] User
             -- :<|> "user"  :> Capture "x" Int :> Get '[JSON] (Maybe User)
             -- :<|> "login" :> ReqBody '[JSON] UserLogin :> Post '[JSON] (Maybe User)

ankiServer :: AnkiDB -> Server AnkiServer
ankiServer ankiDB =  ankiList
  where ankiList  = liftIO $ ankiCards ankiDB


initializeAnkiDB :: FilePath -> IO AnkiDB
initializeAnkiDB = initializeAnkiCards
