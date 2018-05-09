{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- This module contains both specific validation functions for handling Google
-- JWTs, and a Servant Generalized Authentication handler as documented here:
--
-- http://haskell-servant.readthedocs.io/en/stable/tutorial/Authentication.html#generalized-authentication-in-action
module Auth where

import Types

import           Control.Concurrent               (readMVar)
import           Control.Monad.Except
    (MonadError, MonadIO, liftIO, runExceptT, throwError)
import           Crypto.JWT
    ( JWKSet
    , JWTError
    , decodeCompact
    , defaultJWTValidationSettings
    , verifyClaims
    )
import qualified Data.ByteString.Char8            as B8
import qualified Data.ByteString.Lazy.Char8       as L8
import           Data.Monoid                      ((<>))
import           Data.String                      (fromString)
import qualified Data.Text                        as T
import           Network.Wai                      (Request, requestHeaders)
import           Servant                          (err401, errBody)
import           Servant.Server.Experimental.Auth (AuthHandler, mkAuthHandler)

validateJwt :: (MonadError String m, MonadIO m) => T.Text -> JWKSet -> B8.ByteString -> m Account
validateJwt clientId keyset bearerCreds = do
  -- These settings will verify that the JWT was issued in response to an OAuth
  -- request that used our client ID. That cliend ID is public, but is
  -- restricted to our specific domains that Google will allow authentication
  -- from.
  let config = defaultJWTValidationSettings (== (fromString . T.unpack $ clientId))

  -- "compact" is the name for a base64 encoded JWT. Since the raw
  -- Authorization header is passed in, we need to drop the leading "Bearer "
  -- text before decoding.
  --
  -- TODO: Come back and either remove or explain the liftIO . runExceptT
  verifiedJwt <- liftIO . runExceptT $
        decodeCompact (L8.fromStrict $ B8.drop (B8.length "Bearer ") bearerCreds)
    >>= verifyClaims config keyset

  -- TODO: Extract email from JWT and put in account.
  case verifiedJwt of
    Left (e :: JWTError) -> throwError ("Could not verify JWT: " <> show e)
    Right _              -> return Account


-- A servant Generalized Authorization handler. Will 401 unless a valid JWT is present.
handler :: AppConfig -> AuthHandler Request Account
handler config = mkAuthHandler f
  where
    f req = do
      keyset <- liftIO . readMVar $ cnfJwk config
      account <- liftIO . runExceptT $
            maybeToError "No Authorization header present" (lookupReqHeader "Authorization" req)
        >>= validateJwt (cnfOauthClientId config) keyset

      either throw401 return account

    lookupReqHeader h = lookup h . requestHeaders
    throw401 s = throwError err401 { errBody = L8.pack s }

    maybeToError :: (MonadError e m) => e -> Maybe a -> m a
    maybeToError s = maybe (throwError s) (return)
