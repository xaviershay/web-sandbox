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
import Data.String (fromString)
import Control.Concurrent
import Control.Applicative
import Data.Time.Clock
import Text.ParserCombinators.ReadP
import Data.Char
import Network.HTTP.Types.Header

import Control.Monad.Except (runExceptT, MonadError)

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8

import qualified Data.HashMap.Strict as HM

import Lib
import Types
import Auth
import Api
import JsGeneration

serverContext :: AppConfig -> Servant.Context (AuthHandler Network.Wai.Request Account ': '[])
serverContext config = authHandler config :. EmptyContext

corsPolicy = simpleCorsResourcePolicy
               { corsRequestHeaders = [ "authorization", "content-type" ]
               }

app :: AppConfig -> Application
app config = logStdoutDev $
             cors (const . Just $ corsPolicy) $
             provideOptions myApiProxy $
             serveWithContext myApiProxy (serverContext config) myApi


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

focus = main
