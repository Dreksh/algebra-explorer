module UI.Notification exposing (
    Model, Event, init, update, view,
    advance, displayError,
    encode, decoder
    )

import Dict
import Html exposing (Html, div, pre, text)
import Html.Attributes exposing (class, style)
import Html.Keyed exposing (node)
import Json.Encode as Encode
import Json.Decode as Decode
import Process
import Task
-- Our modules
import Helper
import UI.Animation as Animation
import UI.HtmlEvent
import UI.Icon as Icon

notificationDuration_: Float
notificationDuration_ = 15000

animationDuration_: Float
animationDuration_ = 750

maxHeight_: Float
maxHeight_ = 8

type alias Model =
    {   nextID: Int
    ,   notifications: Dict.Dict Int (Bool, String, Animation.EaseState Float) -- Bool represents whether it's deleting
    }

type Event = ClearEvent Int

init: Model
init = {nextID = 0, notifications = Dict.empty}

advance: Float -> Model -> Model
advance time model =
    {   model
    |   notifications = model.notifications
        |> Dict.map (\_ (a,b,height) -> (a,b,Animation.advance time height))
        |> Dict.filter (\_ (deleting, _, height) -> not deleting || Animation.current height /= 0)
    }

displayError: String -> (Model, Animation.Tracker, Cmd Event) -> (Model, Animation.Tracker, Cmd Event)
displayError str (model, tracker, others) =
    let (height, newT) = Animation.newEaseFloat animationDuration_ 0 |> Animation.setEase tracker maxHeight_ in
    (   {   model
        |   nextID = model.nextID + 1
        ,   notifications = Dict.insert model.nextID (False, str, height) model.notifications
        }
    ,   newT
    ,   Cmd.batch [others, Task.perform (\_ -> ClearEvent model.nextID) (Process.sleep notificationDuration_)]
    )

update: Animation.Tracker -> Event -> Model -> (Model, Animation.Tracker)
update tracker e model = case e of
    ClearEvent id -> case Dict.get id model.notifications of
        Just (False, str, height) -> let (newHeight, newT) = Animation.setEase tracker 0 height in
            (   {   model
                |   notifications = Dict.insert id (True, str, newHeight) model.notifications
                }
            ,   newT
            )
        _ -> (model, tracker)

view: (Event -> msg) -> List (Html.Attribute msg) -> Model -> Html msg
view converter attrs model = node "div" attrs
    (   Dict.toList model.notifications
        |> List.map (\(id, val) -> notificationDiv_ converter id val)
    )

notificationDiv_: (Event->msg) -> Int -> (Bool, String, Animation.EaseState Float) -> (String, Html msg)
notificationDiv_ converter id (_, str, height) =
    (   "notification-" ++ (String.fromInt id)
    ,   div
        [   class "notificationMessage"
        ,   UI.HtmlEvent.onClick (ClearEvent id)
        ,   style "max-height" ((Animation.current height |> String.fromFloat) ++ "rem")
        ]
        [   Icon.cancel [Icon.class "clickable", Icon.class "cancelable"]
        ,   pre [] [text str]
        ]
        |> Html.map converter
    )

encode: Model -> Encode.Value
encode model = Encode.object
    [   ("nextID", Encode.int model.nextID)
    ,   ("notifications", Encode.dict String.fromInt encodeNotification_ model.notifications )
    ]

encodeNotification_: (Bool, String, Animation.EaseState Float) -> Encode.Value
encodeNotification_ (deleting, str, _) = Encode.object
    [   ("deleting", Encode.bool deleting)
    ,   ("message", Encode.string str)
    ]

decoder: Decode.Decoder Model
decoder = Decode.map2 (\id n -> {nextID = id, notifications = n})
    (Decode.field "nextID" Decode.int)
    (Decode.field "notifications" <| Helper.intDictDecoder notificationDecoder_)

notificationDecoder_: Decode.Decoder (Bool, String, Animation.EaseState Float)
notificationDecoder_ = Decode.map2
    (\d m ->
        (   d
        ,   m
        ,   Animation.newEaseFloat animationDuration_ (if d then 0 else maxHeight_)
        )
    )
    (Decode.field "deleting" Decode.bool)
    (Decode.field "message" Decode.string)