module UI.Input exposing (Model, Event, init, update, view, open, close, decoder, encode)

import Html
import Html.Attributes exposing (class, id, name, placeholder, type_, value)
import Json.Decode as Decode
import Json.Encode as Encode
-- Ours
import Helper
import UI.Animation as Animation
import UI.HtmlEvent as HtmlEvent
import UI.Icon as Icon
import UI.Animation as Animation
import UI.Animation as Animation

type alias Model =
    {   options: List Selection
    ,   openCount: Int
    ,   displayOptions: Bool
    ,   selected: Maybe (String, Bool) -- The bool is to toggle to signal when state is changing
    ,   existing: Maybe (Animation.DeletableElement Int Event)
    }

type Selection =
    Default String
    | Previous String

type Event =
    Click String
    | Clear Int
    | Submit String
    | ShowOptions

defaultOptions_: List Selection
defaultOptions_ =
    [   Default "x+4=5"
    ,   Default "\\f(x)=x+3"
    ,   Default "x(x+2)=-1"
    ,   Default "4x+3y=11"
    ,   Default "2x+y=5"
    ]

init: Bool -> Model
init show =
    {   options = defaultOptions_
    ,   openCount = if show then 1 else 0
    ,   displayOptions = False
    ,   selected = Nothing
    ,   existing = if show then Animation.newDeletable (closeCmd_ 0) 0 |> Just else Nothing
    }

open: Model -> Model
open model = case model.existing of
    Nothing -> {model | openCount = model.openCount + 1, existing = Animation.newDeletable (closeCmd_ model.openCount) model.openCount  |> Just }
    Just e -> if e.deleting
        then {model | openCount = model.openCount + 1, existing = Animation.newDeletable (closeCmd_ model.openCount) model.openCount  |> Just  }
        else model

close: Model -> (Model, Cmd Event)
close model = case model.existing of
    Nothing -> (model, Cmd.none)
    Just e -> let (newE, cmd) = Animation.delete e in
        ({model | existing = Just newE}, cmd)

closeCmd_: Int -> () -> Cmd Event
closeCmd_ num _ = Animation.delayEvent 500 (Clear num)

update: Event -> Model -> (Model, String, Cmd Event)
update event model = case event of
    Click input -> case model.selected of
        Nothing -> ({model | selected = Just (input, False)}, "", Cmd.none)
        Just (_, token) -> ({model | selected = Just (input, not token)}, "", Cmd.none)
    Submit input -> case model.existing of
        Nothing -> (model, "", Cmd.none)
        Just e -> Animation.delete e
            |> \(newE, cmd) -> ({model | existing = Just newE, selected = Nothing, options = Previous input :: model.options}, input, cmd)
    Clear id -> case model.existing of
        Just e -> if e.element == id then ({model | existing = Nothing}, "", Cmd.none) else (model, "", Cmd.none)
        _ -> (model, "", Cmd.none)
    ShowOptions -> ({model | displayOptions = True}, "" ,Cmd.none)

view: (Event -> msg) -> Model -> Maybe (String, Html.Html msg)
view converter model = model.existing
    |> Maybe.map (\e ->
        (   "textbar"++String.fromInt e.element
        ,    Html.div
            [class "input"]
            (List.filterMap identity
                [   Html.form
                    (   [class "textbar", HtmlEvent.onSubmitField "equation" Submit]
                    |>  Helper.maybeAppend (Animation.class e)
                    )
                    [   Icon.equation []
                    ,   Html.input
                        (   [ type_ "text"
                            , name "equation"
                            , id "textInput"
                            , placeholder "Type an equation to solve"
                            , HtmlEvent.onFocus ShowOptions
                            ]
                            |> Helper.maybeAppend (Maybe.map (\(text, _) -> value text) model.selected)
                        )
                        []
                    ]
                    |> Html.map converter
                    |> Just
                ,   Html.ul []
                    (   List.map (\entry -> case entry of
                            Default val -> Html.li [HtmlEvent.onClick (Click val |> converter)]
                                [   Icon.default []
                                , Html.a [class "clickable"] [Html.text val]
                                ]
                            Previous val -> Html.li [HtmlEvent.onClick (Click val |> converter)]
                                [   Icon.history []
                                , Html.a [class "clickable"] [Html.text val]
                                ]
                        ) model.options
                    )
                    |> Helper.maybeGuard model.displayOptions
                ]
            )
        )
    )

encode: Model -> Encode.Value
encode model = Encode.object
    [   ("options", Encode.list Encode.string <| List.filterMap (\e -> case e of
            Default _ -> Nothing
            Previous value -> Just value
            ) model.options
        )
    ,   ("open", Encode.int model.openCount)
    ,   ("showOptions", Encode.bool model.displayOptions)
    ,   ("selected", case model.selected of
            Nothing -> Encode.null
            Just (val, token) -> Encode.object [("value", Encode.string val), ("token", Encode.bool token)]
        )
    ,   ("existing", case model.existing of
            Nothing -> Encode.null
            Just e -> Animation.encode Encode.int e
        )
    ]

decoder: Decode.Decoder Model
decoder = Decode.map5 Model
    (Decode.field "options" <| Decode.map (\prev -> List.map Previous prev ++ defaultOptions_) <| Decode.list <| Decode.string)
    (Decode.field "open" Decode.int)
    (Decode.field "showOptions" <| Decode.bool)
    (Decode.maybe <| Decode.field "selected" <|
        Decode.map2 Tuple.pair (Decode.field "value" Decode.string) (Decode.field "token" Decode.bool)
    )
    (Decode.maybe <| Decode.field "existing" <| Animation.decoder Decode.int closeCmd_)