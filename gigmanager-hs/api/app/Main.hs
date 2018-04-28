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

import qualified Data.Text as T
import Data.Text.IO as T (writeFile, readFile)
import Data.Aeson
import Data.Maybe (fromJust)
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
import Network.HTTP.Media ((//), (/:))
import Data.String (fromString)
import Control.Concurrent

import Control.Monad.Except (runExceptT)

import Crypto.JWT
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

type HTML = String

instance Accept HTML where
  contentType _ = "text" // "html" /: ("charset", "utf-8")

instance MimeRender HTML String where
  mimeRender _ = L8.pack

type MyAPI = AuthProtect "google-jwt" :> "quotes" :> Get '[JSON] [Quote]
        :<|> "quotes" :> Capture "id" QuoteId :> Get '[JSON] Quote
        -- :<|> "authorize" :> Get '[HTML] HTML
        -- :<|> "authorized" :> QueryParam "code" String :> Get '[HTML] HTML

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

getAuthorize :: Handler HTML
getAuthorize = return $ concat ["<h1><a href=", authEndpoint google, ">", "Get Authorized!", "</a></h1>"]

getAuthorized :: Maybe String -> Handler HTML
getAuthorized mcode = do
  case mcode of
    Nothing -> error "You must pass in a code as a parameter"
    Just code -> do
      stuff <- liftIO $ getAccessToken code
      return $ case stuff of
        Nothing ->  "<h1>Error Fetching Token</h1>"
        Just (t, jwt) -> concat ["<h1>Your Token Is:</h1>", "<h3>" , t , "</h3>"
                         , "JWT: ", jwt]

getAccessToken :: String -> IO (Maybe (String, String))
getAccessToken code = do
  let endpoint = tokenEndpoint code google
  request' <- parseRequest endpoint
  let request = setRequestMethod "POST"
                $ addRequestHeader "Accept" "application/json"
                $ setRequestQueryString [("client_id", Just . B8.pack . oauthClientId $ google)
                                        , ("client_secret", Just . B8.pack . oauthClientSecret $ google)
                                        , ("redirect_uri", Just "http://localhost:8000/authorized")
                                        , ("grant_type", Just "authorization_code")
                                        , ("code", Just . B8.pack $ code)]
                $ request'
  response <- httpJSONEither request
  return $ case (getResponseBody response :: Either JSONException Object) of
             Left _ -> Nothing
             Right obj -> case (HM.lookup "access_token" obj, HM.lookup "id_token" obj) of
                            (Just (String x), Just (String jwt)) ->
                              Just (T.unpack x, T.unpack jwt)

myApiProxy :: Proxy MyAPI
myApiProxy = Proxy

data Account = Account

validateJwt :: JWKSet -> B8.ByteString -> Handler Account
validateJwt keyset bearerCreds = do
  let config = defaultJWTValidationSettings (== (fromString $ oauthClientId google))
  --
  -- TODO: Remove Right
  let Right unverifiedJwt = decodeCompact (L8.fromStrict $ B8.drop (B.length "Bearer ") bearerCreds) :: Either JWTError SignedJWT

  verifiedJwt :: Either JWTError ClaimsSet <- liftIO . runExceptT $ verifyClaims config keyset unverifiedJwt

  --liftIO . putStrLn . show $ verifiedJwt
  case verifiedJwt of
    Left e -> throwError err401
    Right claims -> return Account

authHandler :: AppConfig -> AuthHandler Network.Wai.Request Account
authHandler config = mkAuthHandler handler
  where
    handler req = do
      keyset <- liftIO . readMVar $ cnfJwk config
      case lookup "Authorization" . requestHeaders $ req of
        Just jwt -> validateJwt keyset jwt
        Nothing -> throwError err401

      liftIO . putStrLn . show . lookup "Authorization" . requestHeaders $ req
      return Account

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

main :: IO ()
main = do
  --let jsApi = jsForAPI myApiProxy .  reactWith $
  --              defCommonGeneratorOptions { urlPrefix = "http://localhost:8000"}

  --T.writeFile "../frontend/src/ApiFunctions.js" jsApi
  --putStrLn $ authEndpoint google

  Just jwkData <- decode <$> (L.readFile $ "google-public-key.jwk")

  jwkVar <- newMVar jwkData
  let config = AppConfig { cnfJwk = jwkVar }

  run 8000 (app config)
  -- Need to strip off any trailing whitespace
  --contents <- L.reverse . L.drop 1 . L.reverse <$> L.readFile "test.jwt"
  --let maybeJwt = decodeCompact contents :: Either JWTError SignedJWT
  --case maybeJwt of
  --  Right jwt -> do
  --    Just jwk <- decode <$> L.readFile "google-public-key.jwk"
  --    let x = jwk :: JWKSet
  --    let config = defaultJWTValidationSettings (\_ -> True)
  --    y :: Either JWTError ClaimsSet <- runExceptT $ verifyClaims config jwk jwt
  --    let Right claims = y

  --    putStrLn . show $ (claims ^. unregisteredClaims) ^.at "email"
  --    --putStrLn . show $ x
  --  Left x -> putStrLn . show $ x

