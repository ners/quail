{-# LANGUAGE CPP #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-missing-role-annotations #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}
{-# OPTIONS_GHC -Wno-term-variable-capture #-}

module Main where

import Control.Lens.Operators
import Control.Monad (liftM2)
import Dashi.Components.ActionBar
import Dashi.Components.Button
import Dashi.Components.Form
import Dashi.Components.TextField
import Dashi.Components.Util (ariaBusy_)
import Dashi.Components.Widget
import Dashi.Layout.Page (Page (..))
import Dashi.Prelude
import Dashi.Style.Tokens hiding (Success)
import Dashi.Style.Tokens qualified as Dashi
import Data.Generics.Labels ()
import GraphQL qualified
import Miso
import Miso.GraphQL.JSON qualified as GraphQL
import Miso.Html.Element (div_, form_)
import Miso.Html.Event (onSubmit)
import Miso.Html.Property (disabled_)
import Miso.JSON (Result (..))

#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif

main :: IO ()
main = do
    URI{..} <- getURI
    let updateModel :: Action -> Effect parent Model Action
        updateModel = liftM2 (>>) traceAction appUpdate
        initialModel =
            Model
                { stub = Nothing
                , target = Nothing
                , stubRequest = NotRequested
                , submitRequest = NotRequested
                }
    run
        . startApp (defaultEvents <> keyboardEvents)
        $ (component initialModel updateModel appView)
            { initialAction = Just $ SetStub uriPath
            }

data RequestStatus req res
    = NotRequested
    | RequestInProgress req
    | ResponseReceived (Result res)
    deriving stock (Eq, Show, Generic)

isRequestInProgress :: RequestStatus req res -> Bool
isRequestInProgress RequestInProgress{} = True
isRequestInProgress _ = False

data Model = Model
    { stub :: Maybe MisoString
    , target :: Maybe MisoString
    , stubRequest :: RequestStatus MisoString Bool
    , submitRequest :: RequestStatus () ()
    }
    deriving stock (Eq, Generic, Show)

data Action
    = NoOp
    | SetStub MisoString
    | SetTarget MisoString
    | Submit
    | StubResponseReceived (Result Bool)
    | SubmitResponseReceived (Result ())
    deriving stock (Show)

traceAction :: Action -> Effect parent model action
traceAction = io_ . consoleLog . fromString . ("action: " <>) . show

appUpdate :: Action -> Effect parent Model Action
appUpdate NoOp = pure ()
appUpdate (SetStub stub) = do
    #stub ?= stub
    GraphQL.execute
        GraphQL.QueryIsStubAvailable{stub}
        "/gql"
        []
        (StubResponseReceived . (.body))
        (StubResponseReceived . Error . (.body))
appUpdate (SetTarget s) = #target ?= s
appUpdate Submit = #submitRequest .= RequestInProgress ()
appUpdate (StubResponseReceived r) = #stubRequest .= ResponseReceived r
appUpdate (SubmitResponseReceived r) = #submitRequest .= ResponseReceived r

appView :: Model -> View Model Action
appView Model{..} =
    widget @(Page Model Action)
        Page
            { banner = Nothing
            , topBar = Nothing
            , sideNav = Nothing
            , main_ =
                [ div_ [] [text "Hoi!"]
                , form_
                    [onSubmit Submit]
                    [ widget' @(FormField (TextField Action) Model Action)
                        disabledAttrs
                        FormField
                            { legend = [text "Stub"]
                            , required = True
                            , field = stubInput
                            , messages = stubMessages
                            }
                    , widget' @(FormField (TextField Action) Model Action)
                        disabledAttrs
                        FormField
                            { legend = [text "Target"]
                            , required = True
                            , field = targetInput
                            , messages = targetMessages
                            }
                    , widget @(ActionBar Model Action)
                        ActionBar
                            { left =
                                [ widget' @(Button Model Action)
                                    (busyAttrs <> [disabled_ | buttonDisabled])
                                    Button
                                        { size = DefaultSize
                                        , appearance = Primary
                                        , label = [text "Submit"]
                                        }
                                ]
                            , centre = []
                            , right = []
                            }
                    ]
                ]
            , aside = Nothing
            }
  where
    busyAttrs = [ariaBusy_ True | isRequestInProgress submitRequest]
    disabledAttrs = [disabled_ | isRequestInProgress submitRequest]
    requiredInputMessage :: [(Appearance, MisoString)]
    requiredInputMessage = [(Danger, "Required input")]
    stubMessages =
        case stubRequest of
            _ | stub == Just "" -> requiredInputMessage
            _ | RequestInProgress{} <- submitRequest -> []
            NotRequested -> []
            RequestInProgress{} -> [(Subtle, "Checking...")]
            ResponseReceived (Success True) -> [(Dashi.Success, "Available")]
            ResponseReceived (Success False) -> [(Danger, "Not available")]
            ResponseReceived (Error msg) -> [(Danger, msg)]
    stubInput =
        TextField
            { name = "stub"
            , type' = Text
            , value = stub
            , isValid = none ((Danger ==) . fst) stubMessages
            , onChange = SetStub
            }
    targetMessages
        | target == Just "" = requiredInputMessage
        | otherwise = []

    targetInput =
        TextField
            { name = "target"
            , type' = Text
            , value = target
            , isValid = none ((Danger ==) . fst) targetMessages
            , onChange = SetTarget
            }
    buttonDisabled =
        not stubInput.isValid
            || not targetInput.isValid
            || isRequestInProgress submitRequest
