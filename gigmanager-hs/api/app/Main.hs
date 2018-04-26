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
-- import Data.Aeson.Compat
import Data.Aeson.Types
import Data.Time
import Servant
import Servant.API
import Network.Wai
import Network.Wai.Handler.Warp
import GHC.Generics
import Servant.JS
import Servant.Foreign
import Servant.JS.Internal
import Data.Monoid ((<>), mconcat)
import Control.Lens

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

type MyAPI = "quotes" :> Get '[JSON] [Quote]
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

myApi :: Server MyAPI
myApi =   return quotesList
     :<|> quotesGet

app :: Application
app = logStdoutDev $
      serve myApiProxy myApi

focus = main

main :: IO ()
main = do
  let jsApi = jsForAPI myApiProxy .  reactWith $
                defCommonGeneratorOptions { urlPrefix = "http://localhost:8000"}

  T.writeFile "../frontend/src/ApiFunctions.js" jsApi

  run 8000 $ simpleCors $ app
