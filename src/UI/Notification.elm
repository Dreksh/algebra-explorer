module UI.Notification exposing (
    Model, Event, init, update, view,
    displayError,
    encode, decoder
    )

import Dict
import Html exposing (Html, div, pre, text)
import Html.Attributes exposing (class)
import Html.Keyed exposing (node)
import Json.Encode as Encode
import Json.Decode as Decode
-- Our modules
import Helper
import UI.Animation exposing (DeletableElement, newDeletable, delete, delayEvent)
import UI.HtmlEvent
import UI.Icon as Icon

type alias Model =
    {   nextID: Int
    ,   notifications: Dict.Dict Int (DeletableElement String Event) -- Bool represents whether it's deleting
    }

type Event =
    ClearEvent Int
    | DeleteEvent Int

init: Model
init = {nextID = 0, notifications = Dict.empty}

displayError: String -> (Model, Cmd Event) -> (Model, Cmd Event)
displayError str (model, cmd) =
    (   {   model
        |   nextID = model.nextID + 1
        ,   notifications = Dict.insert model.nextID (newDeletable (delayedDelete_  model.nextID) str) model.notifications
        }
    ,   Cmd.batch [ cmd, delayEvent 15000 (ClearEvent model.nextID) ]
    )

delayedDelete_: Int -> () -> Cmd Event
delayedDelete_ id _ = delayEvent 750 (DeleteEvent id) -- Match this with css animation

update: Event -> Model -> (Model, Cmd Event)
update e model = case e of
    ClearEvent id -> case Dict.get id model.notifications of
        Nothing -> (model, Cmd.none)
        Just n -> delete n
            |> \(elem, cmd) -> ({model | notifications = Dict.insert id elem model.notifications}, cmd)
    DeleteEvent id ->
        ({ model | notifications = Dict.remove id model.notifications }, Cmd.none )

view: (Event -> msg) -> List (Html.Attribute msg) -> Model -> Html msg
view converter attrs model = node "div" attrs
    (   Dict.toList model.notifications
        |> List.map (\(id, val) -> notificationDiv_ converter id val)
    )

notificationDiv_: (Event->msg) -> Int -> DeletableElement String Event -> (String, Html msg)
notificationDiv_ converter id notification =
    (   "notification-" ++ (String.fromInt id)
    ,   div
        ([class "notificationMessage", UI.HtmlEvent.onClick (ClearEvent id)] |> Helper.maybeAppend (UI.Animation.class notification))
        [Icon.cancel [Icon.class "clickable", Icon.class "cancelable"], pre [] [text notification.element]]
        |> Html.map converter
    )

encode: Model -> Encode.Value
encode model = Encode.object
    [   ("nextID", Encode.int model.nextID)
    ,   ("notifications", Encode.dict String.fromInt (UI.Animation.encode Encode.string) model.notifications )
    ]

decoder: Decode.Decoder Model
decoder = Decode.map2 (\id n -> {nextID = id, notifications = n})
    (Decode.field "nextID" Decode.int)
    (Decode.field "notifications" notificationDecoder_)

notificationDecoder_: Decode.Decoder (Dict.Dict Int (DeletableElement String Event))
notificationDecoder_ = Helper.intDictDecoder Decode.value
    |> Decode.andThen (Helper.resultDict (\index val map -> let dec = UI.Animation.decoder Decode.string (delayedDelete_ index) in
        case Decode.decodeValue dec val of
            Ok notification -> Ok (Dict.insert index notification map)
            Err _ -> Err ("failed to deserialise " ++ String.fromInt index)
        )
        Dict.empty
        >> Helper.resultToDecoder
    )