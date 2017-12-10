{-# LANGUAGE OverloadedStrings          #-}
module Configs
    ( Config(..)
    , Environment(..)
    , defaultConfig
    , initializeJwt) where

import           Prelude                              as P
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
import           Data.ByteString                       as B
import qualified Data.Text.Lazy.Encoding               as T
import qualified Data.Text.Lazy                        as TL
import           Data.Aeson                            as A
import qualified Data.ByteString.Char8                 as C
import qualified Data.ByteString.Lazy                  as BL
import           Data.List.Split                       (splitOn)
import           GHC.Word                              (Word8)
import           System.Environment                    (lookupEnv, setEnv)

data Config = Config
    { getEnv    :: Environment
    , port      :: Int
    , logger    :: Middleware
    , corsMW    :: Middleware
    , privKey   :: Maybe Jwk
    , pubKey    :: Maybe Jwk
    , serverKey :: B.ByteString
    , serverCrt :: B.ByteString
    }

data Environment =
    Development
  | Test
  | Production
  deriving (Eq, Show, Read)

defaultConfig :: IO Config
defaultConfig = do
    (Just privateKey)  <- lookupEnv "PrivateKey"
    (Just publicKey)   <- lookupEnv "PublicKey"
    (Just serverK)     <- lookupEnv "ServerKey"
    (Just serverC)     <- lookupEnv "ServerCrt"
    port               <- lookupSetting "PORT" 8080
    env                <- lookupSetting "ENV" Development
    let sk = parserTLSKeys serverK
        sc = parserTLSKeys serverC
    return $ Config
                { getEnv    = env
                , port      = port
                , logger    = setLoggers env
                , corsMW    = appCors
                , privKey   = A.decode (strToWord8s privateKey) :: Maybe Jwk
                , pubKey    = A.decode (strToWord8s publicKey) :: Maybe Jwk
                , serverKey = C.pack sk
                , serverCrt = C.pack sc
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
  setEnv "PrivateKey" privateKey >> setEnv "PublicKey" publicKey


lookupSetting :: Read a => String -> a -> IO a
lookupSetting env def = do
    p <- lookupEnv env
    return $ case p of Nothing -> def
                       Just a  -> read a

strToWord8s :: String -> BL.ByteString
strToWord8s = BL.pack . unpackBytes . C.pack

parserTLSKeys :: String -> String
parserTLSKeys str = join $ extract $ split str
    where split = P.reverse . splitOn "\\n"
          extract (h:b) = P.reverse b
          join = P.concatMap (++"\n")
