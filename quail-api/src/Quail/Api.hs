{-# OPTIONS_GHC -Wno-orphans #-}

module Quail.Api where

import Control.Lens.Operators ((.~), (?~))
import Data.ByteString.Lazy (LazyByteString)
import Data.Data (Proxy (..))
import Data.Function ((&))
import Data.Morpheus.Extra ()
import Data.Morpheus.Types
import Data.OpenApi
    ( OpenApi
    , URL (..)
    , allOperations
    , description
    , info
    , license
    , title
    , url
    , version
    )
import Data.Text (Text)
import Data.Text.Lazy (LazyText)
import GHC.Generics (Generic)
import Servant.API
import Servant.Html
import Servant.Metrics (Metrics)
import Servant.OpenApi (HasOpenApi, toOpenApi)
import Servant.Swagger.UI.Core (SwaggerSchemaUI)
import Prelude

type API = NamedRoutes QuailDocumentedAPI

data QuailDocumentedAPI mode = QuailDocumentedAPI
    { swagger :: mode :- SwaggerSchemaUI "swagger" "swagger.json"
    , metrics :: mode :- "metrics" :> Get '[Metrics] LazyByteString
    , api :: mode :- NamedRoutes QuailAPI
    }
    deriving stock (Generic)

data QuailAPI mode = QuailAPI
    { gql :: mode :- "gql" :> NamedRoutes GqlAPI
    , files :: mode :- RawM
    }
    deriving stock (Generic)

data GqlAPI mode = GqlAPI
    { schema :: mode :- "schema" :> Get '[PlainText] Text
    , playground :: mode :- Get '[HTML] LazyText
    , query :: mode :- ReqBody '[JSON] GQLRequest :> Post '[JSON] GQLResponse
    }
    deriving stock (Generic)

apiDoc :: OpenApi
apiDoc =
    toOpenApi (Proxy @(NamedRoutes QuailAPI))
        & info . title .~ "Quail API"
        & info . version .~ "25.12"
        & info . description ?~ "API for Quail, the tiny but tasteful URL shortener"
        & info . license
            ?~ ( "Apache-2.0"
                    & Data.OpenApi.url ?~ URL "https://www.apache.org/licenses/LICENSE-2.0"
               )

instance HasOpenApi RawM where
    toOpenApi _ = desc $ toOpenApi (Proxy @Raw)
      where
        desc :: OpenApi -> OpenApi
        desc =
            allOperations . description
                ?~ "Raw `Application` handler, but with access to the custom monad."
