module Main where

import Control.Applicative ((<|>))
import Control.Lens.Operators ((.~))
import Control.Monad (guard)
import Control.Monad.Extra (fromMaybeM)
import Data.ByteString (ByteString)
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy qualified as LazyByteString
import Data.CaseInsensitive qualified as CI
import Data.Either.Extra (eitherToMaybe)
import Data.Function ((&))
import Data.Morpheus (App)
import Data.Morpheus.Server (httpPlayground)
import Data.Morpheus.Subscriptions (PubApp, httpPubApp, webSocketsApp)
import Data.Morpheus.Types (render)
import Data.String (fromString)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Text.Lazy.Encoding qualified as LazyText
import Data.Time (getCurrentTime)
import Effectful
import Effectful.Error.Static (Error, runErrorWith)
import Effectful.Haxl (Haxl, haxl, runHaxl)
import Effectful.Log (Log, LogLevel (..), LoggerEnv, getLoggerEnv, logMessageIO, object, runLog, (.=))
import Effectful.NonDet (NonDet, OnEmptyPolicy (OnEmptyKeep), emptyEff, runNonDet)
import Effectful.Servant.Generic (runWarpServerSettingsContext)
import Log.Backend.StandardOutput (withStdOutLogger)
import Network.HTTP.Types (status200, status301, status405, statusCode)
import Network.HTTP.Types.URI (queryToQueryText)
import Network.Mime (defaultMimeLookup)
import Network.Wai (Request (..), Response, ResponseReceived, responseFile, responseHeaders, responseLBS, responseStatus)
import Network.Wai qualified as Request
import Network.Wai.Handler.Warp (defaultSettings, setHost, setPort)
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets (defaultConnectionOptions)
import Quail.Api
import Quail.Server.Files (inRootDir)
import Quail.Server.Gql (gqlApp)
import Quail.Server.SQLite qualified as SQLite
import Servant (ServerError)
import Servant.Server (Application, Context (..))
import Servant.Server.Generic (AsServerT)
import Servant.Swagger.UI (swaggerSchemaUIServerT)
import System.Directory.Extra (doesDirectoryExist, doesFileExist)
import System.FilePath (takeFileName)
import System.Metrics qualified as EKG
import System.Metrics.Prometheus.Encode.Text qualified as Prometheus
import System.Metrics.Prometheus.MetricId qualified as Labels
import System.Metrics.Prometheus.Registry qualified as Prometheus (Registry, sample)
import System.Metrics.Prometheus.RegistryT qualified as Prometheus (execRegistryT)
import System.Remote.Monitoring.Prometheus qualified as Prometheus
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

fileServer :: forall es. (Haxl :> es, IOE :> es) => Request -> (Response -> IO ResponseReceived) -> Eff es ResponseReceived
fileServer request respond =
    fromMaybeM (sendFile index) . fmap eitherToMaybe . runNonDet OnEmptyKeep $
        checkInvalidMethod
            <|> tryServePath
            <|> tryServeDirectory
            <|> tryRedirect
  where
    stub = Text.intercalate "/" $ Request.pathInfo request
    index = inRootDir "index.html"
    path = if stub == "/" then index else inRootDir (Text.unpack stub)

    sendPlain status headers =
        liftIO . respond . responseLBS status (("Content-Type", "text/plain") : headers)

    sendFile :: FilePath -> Eff es ResponseReceived
    sendFile f =
        let mimetype = defaultMimeLookup . fromString . takeFileName $ f
         in liftIO . respond $ responseFile status200 [("Content-Type", mimetype)] f Nothing

    checkInvalidMethod, tryServePath, tryServeDirectory, tryRedirect :: Eff (NonDet ': es) ResponseReceived
    checkInvalidMethod = do
        guard $ requestMethod request `notElem` ["GET", "HEAD"]
        sendPlain status405 [] "Only GET or HEAD is supported"

    tryServePath = do
        guard =<< (liftIO . doesFileExist) path
        inject $ sendFile path

    tryServeDirectory = do
        guard =<< (liftIO . doesDirectoryExist) path
        inject $ sendFile index

    tryRedirect = do
        SQLite.UrlEntity{..} <- fromMaybeM emptyEff . haxl $ SQLite.getUrlByStub stub
        sendPlain status301 [("Location", Text.encodeUtf8 target)] ""

server
    :: forall e es
     . (PubApp e, Haxl :> es, IOE :> es)
    => Prometheus.Registry
    -> [e -> Eff es ()]
    -> App e (Eff es)
    -> QuailDocumentedAPI (AsServerT (Eff es))
server registry publish gqlApp =
    QuailDocumentedAPI
        { swagger = swaggerSchemaUIServerT apiDoc
        , metrics =
            Builder.toLazyByteString
                . Prometheus.encodeMetrics
                <$> liftIO (Prometheus.sample registry)
        , api =
            QuailAPI
                { gql = gqlServer publish gqlApp
                , files = fileServer
                }
        }

main :: IO ()
main = do
    store <- EKG.newStore
    EKG.registerGcMetrics store
    registry <- Prometheus.execRegistryT do
        let labels = Labels.fromList [("ghc", "rts")]
            adapterOptions = Prometheus.defaultOptions labels & Prometheus.samplingFrequency .~ 1
        Prometheus.registerEKGStore store adapterOptions
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
                routes = server registry [inject . publish] gqlApp
                context = EmptyContext
            runWarpServerSettingsContext settings context routes middleware
