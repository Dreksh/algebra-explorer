module UI.HistoryView exposing (Event(..), view)

import Dict
import Html
import Html.Attributes exposing (class)
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
    ( case model.selected |> Maybe.andThen (\(eqNum, _) -> Dict.get eqNum model.equations |> Maybe.map (\a -> (eqNum, a))) of
        Nothing -> [Html.text "No history selected"]
        Just (eqNum, eq) -> History.serialize (\current index c children -> let middle = max 0 (List.length children - 1) in
            case List.drop middle children |> List.head of
                Nothing ->[historyEntry_ current (History.SelectPast index |> Display.HistoryEvent eqNum |> DisplayEvent |> converter) (c.root |> Math.toString)]
                Just after -> historyEntry_ current (History.SelectPast index |> Display.HistoryEvent eqNum |> DisplayEvent |> converter) (c.root |> Math.toString)
                    ::  (
                        List.map (Html.div []) (List.take middle children)
                        ++ after
                    )
            ) eq
    )

historyEntry_: Bool -> msg -> String -> Html.Html msg
historyEntry_ current event t = Html.a
    (   if current then [class "selected"] else [ class "clickable", HtmlEvent.onClick event])
    [Html.text t]