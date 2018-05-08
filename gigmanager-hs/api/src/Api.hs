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

module Api where

import Servant
import Servant.API (FromHttpApiData)
import Servant.Foreign (HasForeign, Foreign, foreignFor)
import qualified Data.Text as T
import Servant.Server.Experimental.Auth (AuthServerData)

import Types

-- see https://github.com/sordina/servant-options/issues/2
instance (HasForeign lang ftype api) =>
  HasForeign lang ftype (AuthProtect k :> api) where

  type Foreign ftype (AuthProtect k :> api) = Foreign ftype api

  foreignFor lang Proxy Proxy subR =
    foreignFor lang Proxy (Proxy :: Proxy api) subR

type instance AuthServerData (AuthProtect "google-jwt") = Account

instance FromHttpApiData QuoteId where
  parseUrlPiece x = QuoteId <$> parseUrlPiece x

type MyAPI =      AuthProtect "google-jwt" :> "quotes" :> Get '[JSON] [Quote]
             :<|> "quotes" :> Capture "id" QuoteId :> Get '[JSON] Quote

myApiProxy :: Proxy MyAPI
myApiProxy = Proxy

myApi :: Server MyAPI
myApi =   (\account -> return quotesList)
     :<|> quotesGet

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
