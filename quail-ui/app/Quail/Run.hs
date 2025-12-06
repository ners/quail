{-# LANGUAGE CPP #-}

module Quail.Run
    ( -- ** Live reload
      run
    )
where

import Language.Javascript.JSaddle hiding (Callback)
import Prelude

#ifdef WASM
import qualified Language.Javascript.JSaddle.Wasm as J
#elif !GHCJS_BOTH
import Data.ByteString.Char8 qualified as ByteString
import qualified Language.Javascript.JSaddle.Warp as J
import Data.HashSet (HashSet)
import Data.HashSet qualified as HashSet
import Data.List qualified as List
import Data.Maybe
import Network.Wai qualified as Request
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Text.Encoding qualified as Text
import Debug.Trace (trace)
import Language.Javascript.JSaddle.WebSockets (debugWrapper, jsaddleAppWithJs, jsaddleJs, jsaddleOr)
import System.IO.Unsafe (unsafePerformIO)
import Network.Wai (Middleware)
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setPort, setTimeout)
import Network.Wai.Middleware.RequestLogger (Destination (..), RequestLoggerSettings (..), defaultRequestLoggerSettings, logStdoutDev, mkRequestLogger)
import Network.Wai.Middleware.Static (Policy, isNotAbsolute, noDots, policy, static, staticPolicy, (>->))
import Network.Wai.Middleware.Static qualified as Policy
import Network.WebSockets (defaultConnectionOptions)
import System.Environment
import System.IO (stderr)
import System.Log.FastLogger (fromLogStr, toLogStr)
import Text.Read (readMaybe)
import System.Directory.Recursive (getFilesRecursive)
#endif

-----------------------------------------------------------------------------

{- | Entry point for a miso application
When compiling with jsaddle on native platforms
'run' w-----------------------------------------------------------------------------
ill start a web server for live reload
of your miso application.

When compiling to WASM use 'jsaddle-wasm'.
When compiling to JS no special package is required (simply the 'id' function).
JSM becomes a type synonym for IO
-}
run :: JSM () -> IO ()
#ifdef WASM
run = J.run
#elif GHCJS_BOTH
run = id
#else
run action = do
    port <- fromMaybe 8008 . (readMaybe =<<) <$> lookupEnv "PORT"
    isGhci <- (== "<interactive>") <$> getProgName
    putStrLn $ "Running on port " <> show port <> "..."
    if isGhci
        then debugMiso port action
        else
            runSettings (setPort port (setTimeout 3600 defaultSettings))
                =<< jsaddleOr
                    defaultConnectionOptions
                    (action >> syncPoint)
                    (logStdoutDev . static $ J.jsaddleApp)

{- Reject URIs that are absolute paths
isNotAbsolute :: Policy
isNotAbsolute = predicate $ not . FP.isAbsolute
-}

nonStaticFiles :: HashSet FilePath -> Middleware
nonStaticFiles files app req sendResponse
    | reqFilePath `List.elem` ["", "jsaddle.js"] = app req \res -> do
        log $ "built-in file: " <> reqFilePath
        sendResponse res
    | HashSet.member (Text.unpack reqFilePath) files = app req \res -> do
        log $ "static file: " <> reqFilePath
        sendResponse res
    | otherwise = app (req{Request.pathInfo = []}) \res -> do
        log $ "non-static file: " <> reqFilePath
        sendResponse res
    where
        reqFilePath = Text.intercalate "/" . Request.pathInfo $ req
        log = Text.appendFile "log.txt" . (<> "\n")

-----------------------------------------------------------------------------

{- | Start or restart the server, with a static Middleware policy.

dmj: This is like @debug@ from `jsaddle-warp`, except it uses a static
middleware for static file hosting.

This means that usage of `url('mario.png')` will "just work" when developing
from GHCi.
-}
debugMiso :: Int -> JSM () -> IO ()
debugMiso port f = do
    logger <- mkRequestLogger defaultRequestLoggerSettings{destination = Callback (ByteString.appendFile "log.txt" . (<> "\n") . fromLogStr)}
    staticFiles <- HashSet.fromList . mapMaybe (List.stripPrefix "./") <$> getFilesRecursive "."
    print staticFiles
    debugWrapper $ \withRefresh registerContext ->
        runSettings (setPort port (setTimeout 3600 defaultSettings))
            =<< jsaddleOr
                defaultConnectionOptions
                (registerContext >> f >> syncPoint)
                (logger . nonStaticFiles staticFiles . static . withRefresh . jsaddleAppWithJs . jsaddleJs $ True)

#endif
