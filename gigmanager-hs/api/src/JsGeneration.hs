{-# LANGUAGE OverloadedStrings   #-}

module JsGeneration where

import Servant.JS
import Servant.JS.Internal
import Servant.Foreign
import qualified Data.Text as T
import Control.Lens
import Data.Monoid ((<>))
import Data.Text.IO as T (writeFile)

import Api

writeJsClient :: T.Text -> FilePath -> IO ()
writeJsClient host path = do
  let jsApi = jsForAPI myApiProxy .  reactWith $
                defCommonGeneratorOptions { urlPrefix = host }

  T.writeFile path jsApi

-- "../frontend/src/ApiFunctions.js"
-- "http://localhost:8000"
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
    -- method = req ^. reqMethod
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
