{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Anki.AnkiCard (initializeAnkiCards, AnkiCards, AnkiCard(..), ankiCards) where

import Data.Csv as Csv
import Data.Text    (Text)
import Data.Aeson
import Data.Aeson.TH
import Control.Monad
import GHC.Generics (Generic)
import Servant.Elm  (ElmType)
import Data.Map (Map)
import qualified Data.Map.Lazy as Map
import qualified Control.Concurrent.STM    as T
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V

data AnkiCard = AnkiCard
            { contentEn      :: String
            , contentJpKanji :: String
            , contentJp      :: String
            , contextEn      :: String
            , contextJP      :: String
            , property       :: String
            } deriving (Generic, Show)

$(deriveJSON defaultOptions ''AnkiCard)

type Storage = Map Int AnkiCard
type AnkiCards = T.TMVar Storage

instance FromRecord AnkiCard
instance ToRecord AnkiCard
instance ElmType AnkiCard

initializeAnkiCards :: FilePath -> IO AnkiCards
initializeAnkiCards filepath = do
            csvData <- BL.readFile filepath
            case Csv.decode HasHeader csvData :: Either String (V.Vector AnkiCard) of
                        Left err     -> putStrLn err >> T.atomically (T.newTMVar storage)
                        Right values ->  T.atomically  $ createStorage $ V.toList values
            where  storage = Map.empty
                   createStorage list = T.newTMVar $ foldl zipper storage list


zipper ::  Storage -> AnkiCard -> Storage
zipper storage anki = Map.insert itemId anki storage
            where itemId = (+) 1 $ length storage

ankiCards :: AnkiCards -> IO [AnkiCard]
ankiCards ankiThread = do
            accs <- (T.atomically . T.readTMVar) ankiThread
            return  (Map.elems accs)
