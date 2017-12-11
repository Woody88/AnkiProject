{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

-- AnkiCard Model
module Anki.AnkiCard (initializeAnkiCards, CardID, AnkiCards, AnkiCard(..), ankiCards, createCard, getCard, updateCard) where

import Data.Csv                            as Csv
import Data.Text                           (Text)
import Data.Aeson
import Data.Aeson.TH
import Data.Binary                         as B
import Control.Monad
import Control.Monad.IO.Class              (liftIO)
import GHC.Generics                        (Generic)
import Servant.Server                      (Handler)
import Servant.Elm                         (ElmType)
import qualified Data.ByteString.Lazy      as BL
import Data.Map                            (Map)
import qualified Data.Map.Lazy             as Map
import qualified Control.Concurrent.STM    as T
import qualified Data.Vector               as V

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


ankiCards :: AnkiCards -> Handler [AnkiCard]
ankiCards ankiThread = do
            accs <- liftIO $ (T.atomically . T.readTMVar) ankiThread
            return  (Map.elems accs)

createCard :: AnkiCards -> AnkiCard -> Handler AnkiCard
createCard ankiThread newAnkiCard = do
            cardStorage <- liftIO $ (T.atomically . T.takeTMVar) ankiThread
            let newId = itemId cardStorage
            let cardToInsert = (cardWithId newId)
            let x = makeCard newId  cardToInsert cardStorage
            updatedStorage <- liftIO $ (T.atomically . (T.putTMVar ankiThread)) x
            liftIO $ insertDB cardToInsert >> (return cardToInsert)
            where itemId storage = (+) 1 $ length storage
                  cardWithId id_ = newAnkiCard {cardId = id_}
                  makeCard :: Int -> AnkiCard -> Storage -> Storage
                  makeCard id_ ankiCard storage = Map.insert id_  ankiCard storage

getCard :: AnkiCards -> CardID -> Handler (Maybe AnkiCard)
getCard ankiThread cId = do
            cardStorage <- liftIO $ (T.atomically . T.readTMVar) ankiThread
            return (Map.lookup cId cardStorage)

updateCard :: AnkiCards -> CardID -> AnkiCard -> Handler (Maybe AnkiCard)
updateCard ankiThread cId card = do
            cardStorage <- liftIO $ (T.atomically . T.takeTMVar) ankiThread
            let cardToUpdate = (Map.lookup cId cardStorage)
            case cardToUpdate of
              Nothing    -> liftIO $ putStrLn "Not Found!" >> return Nothing
              Just value -> insertToDB cardStorage >> return (Just card)
            where insertToDB storage = liftIO $ (T.atomically . (T.putTMVar ankiThread)) $ Map.insert cId card storage

insertDB :: AnkiCard -> IO ()
insertDB card = BL.appendFile "anki_list.csv" (Csv.encode [card]) >> putStrLn "New Data Appended!"

zipper ::  Storage -> AnkiCard -> Storage
zipper storage anki = Map.insert itemId anki storage
            where itemId = cardId anki

fetchStorage :: IO Storage
fetchStorage = do
            csvData <- BL.readFile "anki_list.csv"
            case Csv.decode HasHeader csvData :: Either String (V.Vector AnkiCard) of
                        Left err -> putStrLn err >> return storage
                        Right values -> return $ createStorage $ V.toList values
            where  storage = Map.empty
                   createStorage list = foldl zipper storage list

initializeAnkiCards :: FilePath -> IO AnkiCards
initializeAnkiCards filepath = do
       storage <- liftIO $ fetchStorage
       T.atomically $ T.newTMVar storage
