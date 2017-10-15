{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import Common.Types.User
import Common.Api
import           Elm          (Spec (Spec), specsToDir, toElmDecoderSource,
                               toElmEncoderSource, toElmTypeSource)
import           Servant.Elm  (ElmOptions (..), ElmType, Proxy (Proxy),
                               UrlPrefix (Static), defElmImports, defElmOptions,
                               generateElmForAPIWith)

myElmOpts :: ElmOptions
myElmOpts = defElmOptions { urlPrefix = Static "http://localhost:8000" }

spec :: Spec
spec = Spec ["MyApi"]
            (defElmImports
             : toElmTypeSource    (Proxy :: Proxy User)
             : toElmDecoderSource (Proxy :: Proxy User)
             : generateElmForAPIWith myElmOpts (Proxy :: Proxy UserApi))



main :: IO ()
main = specsToDir [spec] "../frontend"
