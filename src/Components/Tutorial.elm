module Components.Tutorial exposing (Model, Event, init, menu, update, view,
    encode, decoder
    )

import Html exposing (Html, div)
import Json.Decode as Decode
import Json.Encode as Encode
-- Ours
import UI.Menu as Menu

type alias Model =
    {   number: Int
    ,   screen: Int
    }

type Event =
    Click

init: Model
init = {number = 0, screen = 0}

update: Event -> Model -> (Model, Cmd Event)
update _ model = (model, Cmd.none)

view: (Event -> msg) -> List (Html.Attribute msg) -> Model -> Html msg
view converter attrs model = div attrs []

menu: (Event -> msg) -> Model -> Menu.Part msg
menu converter model = Menu.Section "Tutorials" True []

encode: Model -> Encode.Value
encode _ = Encode.null

decoder: Decode.Decoder Model
decoder = Decode.succeed init