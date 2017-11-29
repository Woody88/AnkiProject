{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}

module Anki (AnkiDB, AnkiServer, initializeAnkiDB, ankiServer) where

import Data.Csv
import Servant
import Control.Monad.IO.Class
import Anki.AnkiCard (CardID, AnkiCards, AnkiCard(..), ankiCards, createCard, getCard, updateCard, initializeAnkiCards)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V

type AnkiDB = AnkiCards
type AnkiServer = AnkiApi
type AnkiApi = "ankis" :> Get '[JSON] [AnkiCard]
         :<|>  "ankis" :> ReqBody '[JSON] AnkiCard :> Post '[JSON] AnkiCard
         :<|>  "anki"  :> Capture "cardId" CardID  :> Get '[JSON] (Maybe AnkiCard)
         :<|>  "anki"  :> Capture "cardId" CardID  :> ReqBody '[JSON] AnkiCard :> Put '[JSON] (Maybe AnkiCard)


ankiServer :: AnkiDB -> Server AnkiServer
ankiServer ankiDB =  ankiList
                :<|> newAnki
                :<|> anki
                :<|> updateAnki
  where ankiList                  = liftIO $ ankiCards ankiDB
        newAnki newCard           = liftIO $ createCard ankiDB newCard
        anki cardId               = liftIO $ getCard ankiDB cardId
        updateAnki cardId card    = liftIO $ updateCard ankiDB cardId card

initializeAnkiDB :: FilePath -> IO AnkiDB
initializeAnkiDB = initializeAnkiCards
