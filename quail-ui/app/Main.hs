{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-missing-role-annotations #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-term-variable-capture #-}

module Main where

import Control.Lens.Operators
import Control.Monad (liftM2)
import Dashi.Components.Widget
import Dashi.Layout.Page (Page (..))
import Dashi.Style qualified as Style
import Data.Generics.Labels ()
import Data.Maybe (maybeToList)
import Data.String (IsString (fromString))
import GHC.Generics (Generic)
import Language.Javascript.JSaddle qualified as JSaddle
import Miso
import Miso.Html.Element (div_)
import Quail.Run qualified as Quail
import Prelude hiding (init)

#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif

main :: IO ()
main = Quail.run $ startApp app

data Model = Model
    { title :: MisoString
    , url :: Maybe MisoString
    }
    deriving stock (Eq, Generic)

emptyModel :: Model
emptyModel =
    Model
        { title = "Hoi!"
        , url = Nothing
        }

data Action
    = Setup
    | SetUrl MisoString
    | NoOp
    deriving stock (Show)

traceAction :: Action -> Effect parent model action
traceAction = io_ . consoleLog . fromString . ("action: " <>) . show

app :: App Model Action
app = do
    initComponent
        { events = defaultEvents <> keyboardEvents
        , initialAction = Just Setup
        , styles = [Style Style.styleStr, Href "/static/style.css"]
        }
  where
    initComponent :: Component parent Model Action
    initComponent = component emptyModel (liftM2 (>>) traceAction appUpdate) appView

appUpdate :: Action -> Effect parent Model Action
appUpdate Setup = do
    io_ do
        let createElement = JSaddle.js1 @String @String "createElement"
            setAttribute = JSaddle.js2 @String @String @String "setAttribute"
            appendChild :: JSaddle.JSM JSaddle.JSVal -> JSaddle.JSF
            appendChild = JSaddle.js1 @String "appendChild"
        doc <- JSaddle.jsg @String "document"
        head <- doc ^. JSaddle.js @_ @String "head"
        head
            ^. JSaddle.js1 @String @String "getElementsByTagName" "title"
                . JSaddle.js1 @String @Int "item" 0
                . JSaddle.js0 @String "remove"
        head ^. appendChild do
            e <- doc ^. createElement "meta"
            e ^. setAttribute "charset" "utf-8"
            pure e
        head ^. appendChild do
            e <- doc ^. createElement "meta"
            e ^. setAttribute "name" "color-scheme"
            e ^. setAttribute "content" "dark light"
            pure e
        head ^. appendChild do
            e <- doc ^. createElement "meta"
            e ^. setAttribute "name" "viewport"
            e ^. setAttribute "content" "width=device-width, initial-scale=1, shrink-to-fit=no"
            pure e
        head ^. appendChild do
            e <- doc ^. createElement "title"
            e ^. JSaddle.jss @String @String "innerHTML" "Quail"
            pure e
        head ^. appendChild do
            e <- doc ^. createElement "link"
            e ^. setAttribute "rel" "icon"
            e ^. setAttribute "href" "/favicon.ico"
            pure e
        head ^. appendChild do
            e <- doc ^. createElement "link"
            e ^. setAttribute "rel" "icon"
            e ^. setAttribute "href" "/static/icon.svg"
            e ^. setAttribute "type" "image/svg+xml"
            pure e

    io $ maybe NoOp SetUrl <$> (JSaddle.fromJSVal =<< JSaddle.jsg @String "window" ^. JSaddle.js @_ @String "location" . JSaddle.js @_ @String "pathname")
appUpdate (SetUrl url) = #url ?= url
appUpdate NoOp = pure ()

appView :: Model -> View Model Action
appView model =
    widget @(Page Model Action)
        Page
            { banner = Nothing
            , topBar = Nothing
            , sideNav = Nothing
            , main_ =
                [ div_ [] [text model.title]
                , div_ [] $ text <$> maybeToList model.url
                ]
            , aside = Nothing
            }
