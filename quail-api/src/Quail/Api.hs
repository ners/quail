module Quail.Api where

import Control.Lens.Operators ((.~), (?~))
import Data.Data (Proxy (..))
import Data.Function ((&))
import Data.Morpheus.Extra ()
import Data.Morpheus.Types
import Data.OpenApi (OpenApi, URL (..), description, info, license, title, url, version)
import Data.Text (Text)
import Data.Text.Lazy (LazyText)
import GHC.Generics (Generic)
import Servant.API
import Servant.Html
import Servant.OpenApi (toOpenApi)
import Servant.Swagger.UI (SwaggerSchemaUI)
import Prelude

type API = NamedRoutes QuailDocumentedAPI

data QuailDocumentedAPI mode = QuailDocumentedAPI
    { swagger :: mode :- SwaggerSchemaUI "swagger" "swagger.json"
    , api :: mode :- NamedRoutes QuailAPI
    }
    deriving stock (Generic)

data QuailAPI mode = QuailAPI
    { gql :: mode :- "gql" :> NamedRoutes GqlAPI
    , static :: mode :- "static" :> Raw
    , index :: mode :- Get '[HTML] Text
    , url :: mode :- CaptureAll "stub" Text :> Get '[HTML] Text
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
        & info . license ?~ ("Apache-2.0" & Data.OpenApi.url ?~ URL "https://www.apache.org/licenses/LICENSE-2.0")
