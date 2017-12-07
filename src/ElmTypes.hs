{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module ElmTypes where

import           Elm          (Spec (Spec), specsToDir, toElmDecoderSource,
                               toElmTypeSource)
import           Servant.Elm  (ElmType, Proxy (Proxy), defElmImports,
                               generateElmForAPI, generateElmForAPIWith, urlPrefix, defElmOptions, UrlPrefix(..))
import Data.Text.Internal (Text)
import Anki (AnkiServer)
import Anki.AnkiCard (AnkiCard(..))
import Accounts (UserServer)
import Accounts.User (User(..))

--
-- type NameSpace = Text
--
-- elmDir :: FilePath
-- elmDir = "../anki_frontend/src"
--
-- elmNameSpace :: NameSpace
-- elmNameSpace = "Data"
--
-- ankiSpec :: Spec
-- ankiSpec = Spec [elmNameSpace, "AnkiCard"]
--             (defElmImports
--             : toElmTypeSource    (Proxy :: Proxy AnkiCard)
--             : toElmDecoderSource (Proxy :: Proxy AnkiCard)
--             : generateElmForAPI  (Proxy :: Proxy AnkiServer))
--
-- userSpec :: Spec
-- userSpec = Spec [elmNameSpace, "User"]
--             (defElmImports
--             : toElmTypeSource    (Proxy :: Proxy User)
--             : toElmDecoderSource (Proxy :: Proxy User)
--             : generateElmForAPIWith  (defElmOptions { urlPrefix = Dynamic }) (Proxy :: Proxy UserServer))
--
-- main :: IO ()
-- main = specsToDir [ankiSpec, userSpec] elmDir

main :: IO ()
main = putStrLn "Generated!"
