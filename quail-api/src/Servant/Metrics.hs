module Servant.Metrics where

import Data.ByteString.Lazy (LazyByteString)
import Network.HTTP.Media ((//), (/:))
import Servant.API (Accept (..), MimeRender (mimeRender))
import Prelude

data Metrics

instance Accept Metrics where
    contentType _ = "text" // "plain" /: ("version", "0.0.4")

instance MimeRender Metrics LazyByteString where
    mimeRender _ = id
