{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Main where

import qualified System.Posix.Signals as S
import qualified Data.Text as T
import Data.Text.IO as T (writeFile, readFile)
import Data.Aeson
import Data.Maybe (fromJust, listToMaybe)
import Data.Aeson.Types
import Data.Time
import Debug.Trace
import Servant
import Servant.API
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.Servant.Options
import GHC.Generics
import Servant.JS
import Servant.Foreign
import Servant.JS.Internal
import Servant.Auth.Server
import Servant.API.Experimental.Auth
import Servant.Server.Experimental.Auth
import Data.Monoid ((<>), mconcat)
import Control.Lens
import Control.Monad.IO.Class
import Network.HTTP.Simple hiding (Proxy)
import Network.HTTP.Client.TLS
import Network.HTTP.Media ((//), (/:))
import Data.String (fromString)
import Control.Concurrent
import Control.Applicative
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Text.ParserCombinators.ReadP
import Data.Char
import Network.HTTP.Types.Header

import Control.Monad.Except (runExceptT, MonadError)

import Crypto.JWT hiding (string)
import Crypto.JOSE.JWK

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8

import qualified Data.HashMap.Strict as HM

-- see https://github.com/sordina/servant-options/issues/2
instance (HasForeign lang ftype api) =>
  HasForeign lang ftype (AuthProtect k :> api) where

  type Foreign ftype (AuthProtect k :> api) = Foreign ftype api

  foreignFor lang Proxy Proxy subR =
    foreignFor lang Proxy (Proxy :: Proxy api) subR

scope = T.unpack $ T.intercalate " "
  [ "email"
  , "profile"
  , "https://www.googleapis.com/auth/calendar"
  ]

google = OAuth2
  { oauthClientId = "548142593260-q8s4d9d4hsqmb30sg3ljcdd4m2km2nav.apps.googleusercontent.com"
  , oauthClientSecret = "z9Ojsyek9np5tZR5eAvXNW76" -- TODO: Supposed to be secret
  , oauthOAuthorizeEndpoint = "https://accounts.google.com/o/oauth2/v2/auth"
  , oauthAccessTokenEndpoint = "https://www.googleapis.com/oauth2/v4/token"
  , oauthCallback = "http://localhost:8000/authorized"}

data OAuth2 = OAuth2 { oauthClientId :: String
                     , oauthClientSecret :: String
                     , oauthOAuthorizeEndpoint :: String
                     , oauthAccessTokenEndpoint :: String
                     , oauthCallback :: String
} deriving (Show, Eq)

authEndpoint :: OAuth2 -> String
authEndpoint oa = concat [(oauthOAuthorizeEndpoint oa)
                           , "?client_id=", (oauthClientId oa)
                           , "&response_type=", "code"
                           , "&redirect_uri=", (oauthCallback oa)
                           , "&state=state_parameter_passthrough_value"
                           , "&include_granted_scopes=true"
                           , "&access_type=offline"
                           , "&scope=", scope]

tokenEndpoint :: String -> OAuth2 -> String
tokenEndpoint code oa = oauthAccessTokenEndpoint oa

newtype QuoteId = QuoteId Int deriving (Show, Eq, Generic)
newtype QuoteDayId = QuoteDayId Int deriving (Show, Eq, Generic)

instance FromHttpApiData QuoteId where
  parseUrlPiece x = QuoteId <$> parseUrlPiece x

data QuoteDay = QuoteDay {
  quoteDayId        :: QuoteDayId,
  quoteDayOccursOn  :: Day,
  quoteDayNotes     :: T.Text,
  quoteDayCreatedAt :: UTCTime,
  quoteDayUpdatedAt :: UTCTime
} deriving (Show, Generic)

data Quote = Quote {
  quoteId          :: QuoteId,
  quoteDescription :: T.Text,
  quoteDays        :: [QuoteDay],
  quoteCreatedAt  :: UTCTime,
  quoteUpdatedAt  :: UTCTime
} deriving (Show, Generic)

instance ToJSON QuoteId
instance ToJSON QuoteDayId
instance ToJSON QuoteDay
instance ToJSON Quote

type MyAPI = AuthProtect "google-jwt" :> "quotes" :> Get '[JSON] [Quote]
        :<|> "quotes" :> Capture "id" QuoteId :> Get '[JSON] Quote

react :: JavaScriptGenerator
react = reactWith defCommonGeneratorOptions

reactWith :: CommonGeneratorOptions -> JavaScriptGenerator
reactWith opts = mconcat . map (generateReactJSWith opts)

generateReactJSWith :: CommonGeneratorOptions -> AjaxReq -> T.Text
generateReactJSWith opts req =
    "export function " <> fname <> "(" <> argsStr <> ") {\n" <>
    "  return fetch(" <> url <> ");\n" <>
    "}\n\n"
  where
    argsStr = T.intercalate ", " args
    args = captures
        ++ map (view $ queryArgName . argPath) queryparams
    --    ++ body
        ++ map ( toValidFunctionName
               . (<>) "header"
               . view (headerArg . argPath)
               ) hs
    captures = map (view argPath . captureArg)
                 . filter isCapture
                 $ req ^. reqUrl.path

    hs = req ^. reqHeaders
    --body = if isJust(req ^. reqBody)
    --         then [requestBody opts]
    --         else []
    fname = (functionNameBuilder opts $ req ^. reqFuncName)
    method = req ^. reqMethod
    queryparams = req ^.. reqUrl.queryStr.traverse
    url = if url' == "'" then "'/'" else url'
    url' = "'"
       <> urlPrefix opts
       <> urlArgs
       <> queryArgs

    urlArgs = jsSegments
            $ req ^.. reqUrl.path.traverse

    queryArgs = if null queryparams
                  then ""
                  else " + '?" <> jsParams queryparams

quotes =
  [ Quote
      { quoteId = QuoteId 123
      , quoteDescription = T.pack "Zoo"
      , quoteDays = []
      , quoteCreatedAt = read "2011-11-19 18:28:52.607875 UTC"
      , quoteUpdatedAt = read "2011-11-19 18:28:52.607875 UTC"
      }
  ]

quotesList :: [Quote]
quotesList = quotes

quotesGet :: QuoteId -> Handler Quote
quotesGet id = case filter (\q -> quoteId q == id) $ quotes of
                 [] -> throwError err404 { errBody = "no quote with that ID" }
                 (x:_) -> return x

myApiProxy :: Proxy MyAPI
myApiProxy = Proxy

data Account = Account

validateJwt :: (MonadError String m, MonadIO m) => JWKSet -> B8.ByteString -> m Account
validateJwt keyset bearerCreds = do
  let config = defaultJWTValidationSettings (== (fromString $ oauthClientId google))

  verifiedJwt <- liftIO . runExceptT $
        decodeCompact (L8.fromStrict $ B8.drop (B.length "Bearer ") bearerCreds)
    >>= verifyClaims config keyset

  case verifiedJwt of
    Left (e :: JWTError) -> throwError ("Could not verify JWT: " <> show e)
    Right _              -> return Account

toError :: (MonadError e m) => e -> Maybe a -> m a
toError s = maybe (throwError s) (return)

throw401 s = throwError err401 { errBody = L8.pack s }

lookupRequestHeader h = lookup h . requestHeaders

authHandler :: AppConfig -> AuthHandler Network.Wai.Request Account
authHandler config = mkAuthHandler handler
  where
    handler req = do
      keyset <- liftIO . readMVar $ cnfJwk config
      account <- liftIO . runExceptT $
            toError "No Authorization header present" (lookupRequestHeader "Authorization" req)
        >>= validateJwt keyset

      either throw401 return account

type instance AuthServerData (AuthProtect "google-jwt") = Account

serverContext :: AppConfig -> Servant.Context (AuthHandler Network.Wai.Request Account ': '[])
serverContext config = (authHandler config) :. EmptyContext

myApi :: Server MyAPI
myApi =   (\account -> return quotesList)
     :<|> quotesGet
     -- :<|> getAuthorize
     -- :<|> getAuthorized

policy = simpleCorsResourcePolicy { corsRequestHeaders = [ "authorization", "content-type" ] }

app :: AppConfig -> Application
app config = logStdoutDev $
              cors (const $ Just policy) $
              provideOptions myApiProxy $
              serveWithContext myApiProxy (serverContext config) myApi

focus = main

data AppConfig = AppConfig
  { cnfJwk :: MVar JWKSet
  }

computeExpireTime :: POSIXTime -> Network.HTTP.Simple.Response L8.ByteString -> Maybe Int
computeExpireTime now rs =
    let hs              = getResponseHeaders rs
        expires         = do    e <- lookupHeader hExpires hs
                                t <- parseTimeM True defaultTimeLocale "%a, %e %b %Y %T %Z" (B8.unpack e)
                                return . fromIntegral . round $ (utcTimeToPOSIXSeconds t - now)
        cachecontrol    = do    c <- lookupHeader hCacheControl hs
                                d <- readMaxAge $ B8.unpack c
                                return $ d
    in  cachecontrol <|> expires

readMaxAge :: String -> Maybe Int
readMaxAge = fmap fst . listToMaybe . readP_to_S p
    where p = (string "max-age=" >> read <$> munch isDigit) +++ (get >>= const p)

lookupHeader h = listToMaybe . map snd . filter ((h==) . fst)

loadGooglePublicKey :: MVar JWKSet -> IO ()
loadGooglePublicKey mvar = do
  manager <- newTlsManager
  let request = "https://www.googleapis.com/oauth2/v3/certs"
  response <- httpLBS request
  t <- liftIO getPOSIXTime

  if getResponseStatusCode response == 200 then
    do
      let expiresInSeconds = (fromJust $ computeExpireTime t response)
      let body = getResponseBody response

      -- TODO: Better error handling
      let Just jwkData = decode body

      swapMVar mvar jwkData
      -- TODO: Proper logging
      putStrLn $ "Loaded google public key, caching for " <> show expiresInSeconds <> " seconds"
      threadDelay (expiresInSeconds * 1000000)
  else
    do
      putStrLn "Error loading google public key, retrying in 1s"
      threadDelay 1000000

  loadGooglePublicKey mvar

main :: IO ()
main = do
  --let jsApi = jsForAPI myApiProxy .  reactWith $
  --              defCommonGeneratorOptions { urlPrefix = "http://localhost:8000"}

  --T.writeFile "../frontend/src/ApiFunctions.js" jsApi
  --putStrLn $ authEndpoint google


  jwkVar <- newEmptyMVar
  let config = AppConfig { cnfJwk = jwkVar }

  threadId <- forkIO (loadGooglePublicKey jwkVar)
  tid <- myThreadId

  -- Ensure that threads are cleaned up when developing in GHCI. Kill thread is
  -- dangerous, but it's only for dev mode so don't care.
  S.installHandler S.keyboardSignal (S.Catch (killThread threadId >> killThread tid)) Nothing

  run 8000 (app config)
