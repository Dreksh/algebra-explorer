module UI.HistoryView exposing (Event(..), view)

import Dict
import Html
import Html.Attributes exposing (class, id)
-- Ours
import Algo.History as History
import Algo.Math as Math
import Components.Display as Display
import UI.Draggable as Draggable
import UI.HtmlEvent as HtmlEvent

type Event =
    DisplayEvent Display.Event
    | DraggableEvent Draggable.Event

view: (Event -> msg) -> Draggable.Model -> Display.Model -> Html.Html msg
view converter dragModel model = Draggable.div
    (DraggableEvent >> converter) dragModel []
    [ case model.selected |> Maybe.andThen (\(eq, _) -> Dict.get eq model.equations) of
        Nothing -> Html.text "No history selected"
        Just history -> history |> History.serialize (\index c children ->
            Html.div []
            (   Html.a
                [   class "clickable"
                ,   HtmlEvent.onClick (History.SelectPast index |> Display.HistoryEvent |> DisplayEvent |> converter)
                ]
                [Html.text (c.root |> Math.toString)]
            ::  children
            ))
    ]