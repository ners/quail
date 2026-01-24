{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import Control.Lens.Operators ((.~))
import Data.Aeson.Types (ToJSON (..), (.=))
import Data.Aeson.Types qualified as Aeson
import Data.ByteString (ByteString)
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy qualified as LazyByteString
import Data.CaseInsensitive qualified as CI
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
import Effectful
import Effectful.Error.Static (Error, HasCallStack, runErrorWith)
import Effectful.FileSystem (FileSystem, runFileSystem)
import Effectful.Haxl (Haxl, runHaxl)
import Effectful.OpenTelemetry.Log
import Effectful.OpenTelemetry.Trace
import Effectful.Servant.Generic (runWarpServerSettingsContext)
import GHC.Generics (Generic)
import Network.HTTP.Types
    ( queryToQueryText
    , status200
    , status301
    , status405
    , statusCode
    )
import Network.Mime (defaultMimeLookup)
import Network.Wai
    ( Request (..)
    , Response
    , ResponseReceived
    , responseFile
    , responseHeaders
    , responseLBS
    , responseStatus
    )
import Network.Wai qualified as Request
import Network.Wai.Handler.Warp (defaultSettings, setHost, setPort)
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets (defaultConnectionOptions)
import OpenTelemetry.Trace (InstrumentationLibrary)
import Quail.Api
import Quail.Server.Files (inRootDir)
import Quail.Server.Gql (gqlApp)
import Quail.Server.Resolve
import Quail.Server.SQLite (UrlEntity (UrlEntity))
import Quail.Server.SQLite qualified as SQLite
import Servant (ServerError)
import Servant.Server (Context (..))
import Servant.Server.Generic (AsServerT)
import Servant.Swagger.UI (swaggerSchemaUIServerT)
import System.FilePath (takeFileName)
import System.Metrics qualified as EKG
import System.Metrics.Prometheus.Encode.Text qualified as Prometheus
import System.Metrics.Prometheus.MetricId qualified as Labels
import System.Metrics.Prometheus.Registry qualified as Prometheus
    ( Registry
    , sample
    )
import System.Metrics.Prometheus.RegistryT qualified as Prometheus
    ( execRegistryT
    )
import System.Remote.Monitoring.Prometheus qualified as Prometheus
import Prelude

showBs :: ByteString -> Text
showBs = either (Text.pack . show) id . Text.decodeUtf8'

instance ToJSON Request where
    toJSON req =
        Aeson.object
            [ "method" .= showBs req.requestMethod
            , "query"
                .= Aeson.object
                    [ (fromString . Text.unpack $ name) .= value
                    | (name, value) <- queryToQueryText req.queryString
                    ]
            , "url" .= showBs req.rawPathInfo
            , "remote_host" .= show req.remoteHost
            , "user_agent" .= fmap showBs req.requestHeaderUserAgent
            , "body_length" .= show req.requestBodyLength
            , "headers"
                .= Aeson.object
                    [ (fromString . Text.unpack . Text.decodeUtf8 . CI.original $ name)
                        .= Text.decodeUtf8 value
                    | (name, value) <- req.requestHeaders
                    ]
            ]

instance ToJSON Response where
    toJSON res =
        Aeson.object
            [ "status" .= statusCode (responseStatus res)
            , "headers"
                .= Aeson.object
                    [ (fromString . Text.unpack . Text.decodeUtf8 . CI.original $ name)
                        .= Text.decodeUtf8 value
                    | (name, value) <- responseHeaders res
                    ]
            ]

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

data RequestLog = RequestLog {message :: Text, request :: Request}
    deriving stock (Generic)
    deriving anyclass (ToJSON)

fileServer
    :: forall es
     . ( HasCallStack
       , FileSystem :> es
       , Haxl :> es
       , Tracing :> es
       , Logging :> es
       , IOE :> es
       )
    => Request
    -> (Response -> IO ResponseReceived)
    -> Eff es ResponseReceived
fileServer request respond =
    inSpan ("fileServer:" <> stub) spanArguments $
        if requestMethod request `notElem` ["GET", "HEAD"]
            then do
                log_ Trace RequestLog{message = "Invalid method", request} mempty
                sendPlain status405 [] "Only GET or HEAD is supported"
            else
                resolve stub >>= \case
                    Nothing -> sendFile index
                    Just Directory{} -> do
                        log_ Trace RequestLog{message = "Invalid directory request", request} mempty
                        sendFile index
                    Just (File f) -> sendFile f
                    Just (Entity UrlEntity{..}) -> sendPlain status301 [("Location", Text.encodeUtf8 target)] ""
  where
    spanArguments = defaultSpanArguments{kind = Server}
    stub = Text.intercalate "/" . filter (not . Text.null) $ Request.pathInfo request
    index = inRootDir "index.html"

    sendPlain status headers =
        liftIO . respond . responseLBS status (("Content-Type", "text/plain") : headers)

    sendFile :: FilePath -> Eff es ResponseReceived
    sendFile f =
        let mimetype = defaultMimeLookup . fromString . takeFileName $ f
         in liftIO . respond $ responseFile status200 [("Content-Type", mimetype)] f Nothing

server
    :: forall e es
     . ( HasCallStack
       , PubApp e
       , FileSystem :> es
       , Haxl :> es
       , Tracing :> es
       , Logging :> es
       , IOE :> es
       )
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
    SQLite.createUrlDb
    haxlEnv <- SQLite.initUrlEnv
    let instrumentationLibrary :: InstrumentationLibrary
        instrumentationLibrary = "quail-server"
    runEff
        . runFileSystem
        . runHaxl haxlEnv
        . runMainDefaultLogging instrumentationLibrary
        . runMainDefaultTracing instrumentationLibrary
        $ do
            (wsApp, publish) <- runErrorWith @ServerError undefined $ webSocketsApp gqlApp
            let settings = setHost "*6" . setPort 8080 $ defaultSettings
                middleware = websocketsOr defaultConnectionOptions wsApp
                routes
                    :: QuailDocumentedAPI
                        (AsServerT (Eff '[Error ServerError, Tracing, Logging, Haxl, FileSystem, IOE]))

                routes = server registry [inject . publish] gqlApp
                context = EmptyContext
            runWarpServerSettingsContext settings context routes middleware
