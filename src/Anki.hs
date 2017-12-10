{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}

module Anki (AnkiDB, AnkiServer, AnkiEndpoints, initializeAnkiDB, ankiServer) where

import Data.Csv
import Servant
import Servant.Server                   (Handler, err401, err403, err404, errBody, Server)
import Control.Monad.IO.Class
import Anki.AnkiCard (CardID, AnkiCards, AnkiCard(..), ankiCards, createCard, getCard, updateCard, initializeAnkiCards)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
import Auth (Token(..))

type AnkiDB = AnkiCards

type AnkiServer = Server AnkiEndpoints

type AnkiEndpoints =  Get '[JSON] [AnkiCard]
         :<|>  ReqBody '[JSON] AnkiCard :> Post '[JSON] AnkiCard
         :<|>  Capture "cardId" CardID  :> Get '[JSON] (Maybe AnkiCard)
         :<|>  Capture "cardId" CardID  :> ReqBody '[JSON] AnkiCard :> Put '[JSON] (Maybe AnkiCard)


ankiServer :: AnkiDB -> Token -> AnkiServer
ankiServer ankiDB (Token t) =  ankiList
                :<|> newAnki
                :<|> anki
                :<|> updateAnki
  where ankiList                  = ankiCards ankiDB
        newAnki newCard           = createCard ankiDB newCard
        anki cardId               = getCard ankiDB cardId
        updateAnki cardId card    = updateCard ankiDB cardId card

initializeAnkiDB :: FilePath -> IO AnkiDB
initializeAnkiDB = initializeAnkiCards
