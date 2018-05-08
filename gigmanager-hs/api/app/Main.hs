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
import Servant
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.Servant.Options
import Control.Concurrent
import qualified Data.Text.IO as T

import Types
import Auth
import Api
import JsGeneration

app :: AppConfig -> Application
app config = logStdoutDev $
             cors (const . Just $ corsPolicy) $ -- Generate appropriate CORS headers
             provideOptions myApiProxy $        -- Generate OPTIONS handlers for routes
             serveWithContext myApiProxy (serverContext config) myApi

  where
    -- Make our auth handler available as a context. It will be run for routes
    -- tagged with AuthProtect.
    --
    -- http://haskell-servant.readthedocs.io/en/stable/tutorial/Authentication.html#generalized-authentication-in-action
    serverContext config = Auth.handler config :. EmptyContext

    -- Need to explictly allow needed extra headers through CORS.
    corsPolicy = simpleCorsResourcePolicy
                   { corsRequestHeaders = [ "authorization", "content-type" ]
                   }

main :: IO ()
main = do
  writeJsClient "http://localhost:8000" "../frontend/src/ApiFunctions.js"
  clientId <- T.readFile "oauth-client-id"
  config   <- mkConfig clientId

  threadId <- forkIO (loadGooglePublicKey (cnfJwk config))
  tid <- myThreadId

  -- Ensure that threads are cleaned up when developing in GHCI. Kill thread is
  -- dangerous, but it's only for dev mode so don't care.
  S.installHandler S.keyboardSignal (S.Catch (killThread threadId >> killThread tid)) Nothing

  run 8000 (app config)

focus = main
