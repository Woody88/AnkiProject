{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}


module Anki.AnkiCard (initializeAnkiCards, CardID, AnkiCards, AnkiCard(..), ankiCards, createCard, getCard, updateCard) where

import Data.Csv as Csv
import Data.Text    (Text)
import Data.Aeson
import Data.Aeson.TH
import Data.Binary as B
import Control.Monad
import GHC.Generics (Generic)
import Servant.Elm  (ElmType)
import qualified Data.ByteString.Lazy as BL
import Data.Map (Map)
import qualified Data.Map.Lazy as Map
import qualified Control.Concurrent.STM    as T
import qualified Data.Vector as V

data AnkiCard = AnkiCard
            { cardId         :: Int
            , contentEn      :: String
            , contentJpKanji :: String
            , contentJp      :: String
            , contextEn      :: String
            , contextJP      :: String
            , property       :: String
            } deriving (Generic, Show)

$(deriveJSON defaultOptions ''AnkiCard)

type CardID    = Int
type Storage   = Map Int AnkiCard
type AnkiCards = T.TMVar Storage

instance FromRecord AnkiCard
instance ToRecord   AnkiCard
instance ElmType    AnkiCard
instance Binary     AnkiCard

initializeAnkiCards :: FilePath -> IO AnkiCards
initializeAnkiCards filepath = do
            storage <- fetchStorage
            T.atomically $ T.newTMVar storage

zipper ::  Storage -> AnkiCard -> Storage
zipper storage anki = Map.insert itemId anki storage
            where itemId = cardId anki


ankiCards :: AnkiCards -> IO [AnkiCard]
ankiCards ankiThread = do
            accs <- (T.atomically . T.readTMVar) ankiThread
            return  (Map.elems accs)

createCard :: AnkiCards -> AnkiCard -> IO AnkiCard
createCard ankiThread newAnkiCard = do
            cardStorage <- (T.atomically . T.takeTMVar) ankiThread
            let newId = itemId cardStorage
            let cardToInsert = (cardWithId newId)
            let x = makeCard newId  cardToInsert cardStorage
            updatedStorage <- (T.atomically . (T.putTMVar ankiThread)) x
            insertDB cardToInsert >> (return cardToInsert)
            where itemId storage = (+) 1 $ length storage
                  cardWithId id_ = newAnkiCard {cardId = id_}
                  makeCard :: Int -> AnkiCard -> Storage -> Storage
                  makeCard id_ ankiCard storage = Map.insert id_  ankiCard storage

getCard :: AnkiCards -> CardID -> IO (Maybe AnkiCard)
getCard ankiThread cId = do
            cardStorage <- (T.atomically . T.readTMVar) ankiThread
            return (Map.lookup cId cardStorage)

updateCard :: AnkiCards -> CardID -> AnkiCard -> IO (Maybe AnkiCard)
updateCard ankiThread cId card = do
            cardStorage <- (T.atomically . T.takeTMVar) ankiThread
            let cardToUpdate = (Map.lookup cId cardStorage)
            case cardToUpdate of
              Nothing    -> return Nothing
              Just value -> insertToDB cardStorage >> return (Just card)
            where insertToDB storage = (T.atomically . (T.putTMVar ankiThread)) $ Map.insert cId card storage

--updateDiskDB

insertDB :: AnkiCard -> IO ()
insertDB card = BL.appendFile "anki_list.csv" (Csv.encode [card]) >> putStrLn "New Data Appended!"

fetchStorage :: IO Storage
fetchStorage = do
            csvData <- BL.readFile "anki_list.csv"
            case Csv.decode HasHeader csvData :: Either String (V.Vector AnkiCard) of
                        Left err -> putStrLn err >> return storage
                        Right values -> return $ createStorage $ V.toList values
            where  storage = Map.empty
                   createStorage list = foldl zipper storage list
