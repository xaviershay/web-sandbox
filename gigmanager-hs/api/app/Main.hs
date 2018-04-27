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
import Data.Aeson.Types
import Data.Time
import Debug.Trace
import Servant
import Servant.API
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Cors
import GHC.Generics
import Servant.JS
import Servant.Foreign
import Servant.JS.Internal
import Servant.Auth.Server
import Data.Monoid ((<>), mconcat)
import Control.Lens
import Control.Monad.IO.Class
import Network.HTTP.Simple hiding (Proxy)
import Network.HTTP.Media ((//), (/:))

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8

import qualified Data.HashMap.Strict as HM

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

type MyAPI = "quotes" :> Get '[JSON] [Quote]
        :<|> "quotes" :> Capture "id" QuoteId :> Get '[JSON] Quote
        :<|> "authorize" :> Get '[HTML] HTML
        :<|> "authorized" :> QueryParam "code" String :> Get '[HTML] HTML

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
      token <- liftIO $ getAccessToken code
      return $ case token of
        Nothing ->  "<h1>Error Fetching Token</h1>"
        Just t -> concat ["<h1>Your Token Is:</h1>", "<h3>" , t , "</h3>"]

getAccessToken :: String -> IO (Maybe String)
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
             Right obj -> case HM.lookup "access_token" obj of
                            Just (String x) -> Just . T.unpack $ x
             _ -> Nothing

myApiProxy :: Proxy MyAPI
myApiProxy = Proxy

myApi :: Server MyAPI
myApi =   return quotesList
     :<|> quotesGet
     :<|> getAuthorize
     :<|> getAuthorized

app :: Application
app = logStdoutDev $
      serve myApiProxy myApi

focus = main

main :: IO ()
main = do
  --let jsApi = jsForAPI myApiProxy .  reactWith $
  --              defCommonGeneratorOptions { urlPrefix = "http://localhost:8000"}

  --T.writeFile "../frontend/src/ApiFunctions.js" jsApi
  --putStrLn $ authEndpoint google

  run 8000 $ simpleCors $ app
