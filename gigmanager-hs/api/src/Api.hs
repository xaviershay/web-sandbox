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

-- Servant API definition and implementation. In a larger project,
-- implementation would likely be split out into separate files.
module Api where

import Servant
import Servant.Foreign (HasForeign, Foreign, foreignFor)
import qualified Data.Text as T
import Servant.Server.Experimental.Auth (AuthServerData)

import Types

-- Provide the missing HasForeign instance for AuthProtect, such that it is
-- compatible with JS generation and servant-options.
-- see https://github.com/sordina/servant-options/issues/2
instance (HasForeign lang ftype api) =>
  HasForeign lang ftype (AuthProtect k :> api) where

  type Foreign ftype (AuthProtect k :> api) = Foreign ftype api

  foreignFor lang Proxy Proxy subR =
    foreignFor lang Proxy (Proxy :: Proxy api) subR

-- Associate the Account type with any route tagged "google-jwt".
type instance AuthServerData (AuthProtect "google-jwt") = Account

-- The main Servant API type.
type MyAPI =
  -- Routes that need to be protected are tagged with AuthProtect
  AuthProtect "google-jwt" :> "email" :> Get '[JSON] T.Text

  -- Unprotected routes are allowed
  :<|> "unprotected" :> Get '[JSON] T.Text

myApiProxy :: Proxy MyAPI
myApiProxy = Proxy

myApi :: Server MyAPI
myApi =
  -- Protected routes will only be called if a valid account was present.
  (\account -> return (acctEmail account))

  -- Unprotected routes function per normal
  :<|> return "unprotected"
