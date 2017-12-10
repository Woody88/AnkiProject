{-# LANGUAGE OverloadedStrings          #-}
module Configs
    ( Config(..)
    , Environment(..)
    , defaultConfig
    , initializeJwt) where

import           Servant.Server
import           Network.Wai.Middleware.Cors          (CorsResourcePolicy(..), simpleHeaders, simpleMethods, cors)
import           Network.Wai.Middleware.RequestLogger (logStdoutDev, logStdout)
import           Network.Wai                          (Middleware)
import           Control.Monad.Logger                 (runNoLoggingT, runStdoutLoggingT)
import           Jose.Jwk                             (Jwk(..), generateRsaKeyPair, generateSymmetricKey, KeyUse(Enc), KeyId)
import           Jose.Jwt
import           Jose.Jwe
import           Jose.Jwa
import           Data.ByteString.Internal             (unpackBytes)
import qualified Data.Text.Lazy.Encoding               as T
import qualified Data.Text.Lazy                        as TL
import           Data.Aeson                            as A
import qualified Data.ByteString.Char8                 as C
import qualified Data.ByteString.Lazy                  as B
import           GHC.Word                              (Word8)
import           System.Environment                    (lookupEnv, setEnv)

data Config = Config
    { getEnv  :: Environment
    , port    :: Int
    , logger  :: Middleware
    , corsMW  :: Middleware
    , privKey :: Maybe Jwk
    , pubKey  :: Maybe Jwk
    }

data Environment =
    Development
  | Test
  | Production
  deriving (Eq, Show, Read)

defaultConfig :: IO Config
defaultConfig = do
    (Just privateKey) <- lookupEnv "PrivateKey"
    (Just publicKey)  <- lookupEnv "PublicKey"
    port              <- lookupSetting "PORT" 8080
    env               <- Configs.lookupSetting "ENV" Development
    putStrLn ("privateKey: " ++ privateKey)
    putStrLn ("publicKey: " ++ publicKey)
    return $ Config
                { getEnv  = env
                , port    = port
                , logger  = setLoggers env
                , corsMW  = appCors
                , privKey = A.decode (strToWord8s privateKey) :: Maybe Jwk
                , pubKey  = A.decode (strToWord8s publicKey) :: Maybe Jwk
                }

setLoggers :: Environment -> Middleware
setLoggers Test        = id
setLoggers Development = logStdoutDev
setLoggers Production  = logStdout

-- need to allow cors communication with elm
appCors :: Middleware
appCors = cors $ const (Just corsResourcePolicy)

corsResourcePolicy :: CorsResourcePolicy
corsResourcePolicy =
    CorsResourcePolicy
        { corsOrigins = Nothing -- gives you /*
        , corsMethods = simpleMethods
        , corsRequestHeaders = simpleHeaders -- adds "Content-Type" to defaults
        , corsExposedHeaders = Nothing
        , corsMaxAge = Nothing
        , corsVaryOrigin = False
        , corsRequireOrigin = False
        , corsIgnoreFailures = False
        }

initializeJwt :: IO ()
initializeJwt = do
  (kPub, kPr) <- generateRsaKeyPair 512 (KeyId "") Enc Nothing
  let privateKey = TL.unpack $ T.decodeUtf8 $ A.encode kPr
      publicKey  = TL.unpack $ T.decodeUtf8 $ A.encode kPub
  putStrLn ("privateKey: " ++ privateKey)
  putStrLn ("publicKey: " ++ publicKey)
  setEnv "PrivateKey" privateKey >> setEnv "PublicKey" publicKey


lookupSetting :: Read a => String -> a -> IO a
lookupSetting env def = do
    p <- lookupEnv env
    return $ case p of Nothing -> def
                       Just a  -> read a

strToWord8s :: String -> B.ByteString
strToWord8s = B.pack . unpackBytes . C.pack
