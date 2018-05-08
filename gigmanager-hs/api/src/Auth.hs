{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Auth where

import Types

import Crypto.JWT (defaultJWTValidationSettings, decodeCompact, verifyClaims, JWTError, JWKSet)

import Control.Monad.Except (runExceptT, MonadError, MonadIO, throwError, liftIO)
import Control.Concurrent (readMVar, MVar, threadDelay, swapMVar)
import Data.Maybe (fromJust, listToMaybe)
import Data.Monoid ((<>))
import Text.ParserCombinators.ReadP
import Data.String (fromString)
import Data.Char (isDigit)
import Network.Wai (Request, requestHeaders)
import Servant (errBody, err401)
import Servant.Server.Experimental.Auth (AuthHandler, mkAuthHandler)
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as L8

import Data.Time.Format (parseTimeM, defaultTimeLocale)
import Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime, utcTimeToPOSIXSeconds)
import Network.HTTP.Simple hiding (Proxy, Request)
import Network.HTTP.Types.Header (hCacheControl, hExpires)
import Control.Applicative ((<|>))
import Network.HTTP.Client.TLS (newTlsManager)
import Data.Aeson (decode)

oauthClientId = "548142593260-q8s4d9d4hsqmb30sg3ljcdd4m2km2nav.apps.googleusercontent.com"

validateJwt :: (MonadError String m, MonadIO m) => JWKSet -> B8.ByteString -> m Account
validateJwt keyset bearerCreds = do
  let config = defaultJWTValidationSettings (== (fromString oauthClientId))

  verifiedJwt <- liftIO . runExceptT $
        decodeCompact (L8.fromStrict $ B8.drop (B8.length "Bearer ") bearerCreds)
    >>= verifyClaims config keyset

  case verifiedJwt of
    Left (e :: JWTError) -> throwError ("Could not verify JWT: " <> show e)
    Right _              -> return Account

toError :: (MonadError e m) => e -> Maybe a -> m a
toError s = maybe (throwError s) (return)

throw401 s = throwError err401 { errBody = L8.pack s }

lookupRequestHeader h = lookup h . requestHeaders

authHandler :: AppConfig -> AuthHandler Request Account
authHandler config = mkAuthHandler handler
  where
    handler req = do
      keyset <- liftIO . readMVar $ cnfJwk config
      account <- liftIO . runExceptT $
            toError "No Authorization header present" (lookupRequestHeader "Authorization" req)
        >>= validateJwt keyset

      either throw401 return account

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
