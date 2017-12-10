module App where

import           Control.Monad.Except                 (ExceptT)
import           Control.Monad.Reader                 (ReaderT, runReaderT)
import           Servant                              (Handler)
import           Configs                              (Config)

type AppH = ReaderT Config Handler

appHToHandler :: Config -> AppH a -> Handler a
appHToHandler env r = runReaderT r env

returnH :: Config -> AppH a -> Handler a
returnH = appHToHandler
