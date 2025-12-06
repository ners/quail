{-# LANGUAGE CPP #-}

module Main where

import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as LazyByteString
import Data.CaseInsensitive qualified as CI
import Data.Morpheus (App)
import Data.Morpheus.Server (httpPlayground)
import Data.Morpheus.Subscriptions (PubApp, httpPubApp, webSocketsApp)
import Data.Morpheus.Types (render)
import Data.String (fromString)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Text.IO qualified as Text
import Data.Text.Lazy.Encoding qualified as LazyText
import Data.Time (getCurrentTime)
import Effectful
import Effectful.Error.Static (Error, runErrorWith, throwError)
import Effectful.Haxl (Haxl, haxl, runHaxl)
import Effectful.Log (Log, LogLevel (..), LoggerEnv, getLoggerEnv, logMessageIO, object, runLog, (.=))
import Effectful.Servant.Generic (runWarpServerSettingsContext)
import Log.Backend.StandardOutput (withStdOutLogger)
import Network.HTTP.Types (statusCode)
import Network.HTTP.Types.URI (queryToQueryText)
import Network.Wai (Request (..), responseHeaders, responseStatus)
import Network.Wai.Handler.Warp (defaultSettings, setHost, setPort)
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets (defaultConnectionOptions)
import Quail.Api
import Quail.Server.Gql (gqlApp)
import Quail.Server.SQLite qualified as SQLite
import Servant (ServerError (errHeaders), err301, serveDirectoryFileServer)
import Servant.Server (Application, Context (..))
import Servant.Server.Generic (AsServerT)
import Servant.Swagger.UI (swaggerSchemaUIServerT)
import System.FilePath ((</>))
import Prelude

showBs :: ByteString -> Text
showBs = either (Text.pack . show) id . Text.decodeUtf8'

logMiddleware :: LoggerEnv -> Application -> Application
logMiddleware loggerEnv app req respond' = do
    time <- getCurrentTime
    logMessageIO loggerEnv time LogTrace "Request" $
        object
            [ "method" .= showBs req.requestMethod
            , "query"
                .= object
                    [ (fromString . Text.unpack $ name) .= value
                    | (name, value) <- queryToQueryText req.queryString
                    ]
            , "url" .= showBs req.rawPathInfo
            , "remote_host" .= show req.remoteHost
            , "user_agent" .= fmap showBs req.requestHeaderUserAgent
            , "body_length" .= show req.requestBodyLength
            , "headers"
                .= object
                    [ (fromString . Text.unpack . Text.decodeUtf8 . CI.original $ name)
                        .= Text.decodeUtf8 value
                    | (name, value) <- req.requestHeaders
                    ]
            ]
    app req \res -> do
        logMessageIO loggerEnv time LogTrace "Response" $
            object
                [ "status" .= statusCode (responseStatus res)
                , "headers"
                    .= object
                        [ (fromString . Text.unpack . Text.decodeUtf8 . CI.original $ name)
                            .= Text.decodeUtf8 value
                        | (name, value) <- responseHeaders res
                        ]
                ]
        respond' res

gqlServer
    :: (PubApp e, IOE :> es)
    => [e -> Eff es ()]
    -> App e (Eff es)
    -> GqlAPI (AsServerT (Eff es))
gqlServer publish gqlApp =
    GqlAPI
        { schema = pure . Text.decodeUtf8 . LazyByteString.toStrict . render $ gqlApp
        , playground = pure . LazyText.decodeUtf8 $ httpPlayground
        , query = httpPubApp publish gqlApp
        }

server
    :: (PubApp e, Error ServerError :> es, Haxl :> es, IOE :> es)
    => [e -> Eff es ()]
    -> App e (Eff es)
    -> QuailDocumentedAPI (AsServerT (Eff es))
server publish gqlApp =
    QuailDocumentedAPI
        { swagger = swaggerSchemaUIServerT apiDoc
        , api =
            QuailAPI
                { gql = gqlServer publish gqlApp
                , static = serveDirectoryFileServer $ ROOT_DIR </> "static"
                , index = liftIO . Text.readFile $ ROOT_DIR </> "index.html"
                , url = \(Text.intercalate "/" -> stub) ->
                    haxl (SQLite.getUrlByStub stub) >>= \case
                        Nothing -> liftIO . Text.readFile $ ROOT_DIR </> "index.html"
                        Just SQLite.UrlEntity{..} ->
                            throwError $ err301{errHeaders = [("Location", Text.encodeUtf8 target)]}
                }
        }

main :: IO ()
main = do
    let logLevel = LogTrace
    SQLite.createUrlDb
    haxlEnv <- SQLite.initUrlEnv
    withStdOutLogger \logger -> runEff
        . runHaxl haxlEnv
        . runLog "quail" logger logLevel
        $ do
            (wsApp, publish) <- runErrorWith @ServerError undefined $ webSocketsApp gqlApp
            loggerEnv <- getLoggerEnv
            let settings = setHost "*6" . setPort 8081 $ defaultSettings
                middleware = logMiddleware loggerEnv . websocketsOr defaultConnectionOptions wsApp
                routes :: QuailDocumentedAPI (AsServerT (Eff '[Error ServerError, Log, Haxl, IOE]))
                routes = server [inject . publish] gqlApp
                context = EmptyContext
            runWarpServerSettingsContext settings context routes middleware
