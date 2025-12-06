module Servant.Html where

import Data.ByteString (ByteString)
import Data.ByteString.Lazy (LazyByteString)
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Text (Text)
import Data.Text.Encoding qualified as Text
import Data.Text.Lazy (LazyText)
import Data.Text.Lazy.Encoding qualified as LazyText
import Network.HTTP.Media ((//), (/:))
import Servant.API (Accept (..), MimeRender (mimeRender))
import Prelude

data HTML

instance Accept HTML where
    contentType _ = "text" // "html" /: ("charset", "utf-8")

instance MimeRender HTML LazyByteString where
    mimeRender _ = id

instance MimeRender HTML ByteString where
    mimeRender p = mimeRender p . LazyByteString.fromStrict

instance MimeRender HTML Text where
    mimeRender p = mimeRender p . Text.encodeUtf8

instance MimeRender HTML LazyText where
    mimeRender p = mimeRender p . LazyText.encodeUtf8
