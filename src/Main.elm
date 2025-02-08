module Main exposing (main)

import Browser exposing (Document, document)
import Json.Decode exposing (Decoder, Value, decodeValue, field, string)
import Html exposing (div, h1, text)
import Html.Attributes exposing (class)

-- Overall Structure of the app: it's a document

main = document 
    {    init = init
    ,    view = view
    ,    update = update
    ,    subscriptions = subscriptions
    }

-- Types

type alias Model =
    {   message: String
    }

type Event =
    One

-- Events

flagDecoder: Decoder String
flagDecoder = field "message" string

init: Value -> (Model, Cmd Event)
init value =
    let
        message = value
            |> decodeValue flagDecoder
            |> Result.withDefault "Hello!"
    in
        (   { message = message
            }
        , Cmd.none
        )

subscriptions: Model -> Sub Event
subscriptions _ = Sub.none

update: Event -> Model -> ( Model, Cmd Event )
update _ model = (model, Cmd.none)

view: Model -> Document msg
view model =
    { title = "Maths"
    , body =
        [   div
            [   class "horizontalCenter"
            ,   class "verticalCenter"
            ]
            [   h1
                []
                [ text model.message
                ]
            ]
        ]
    }