module Main exposing (Model, Msg, init, main, update, view)

import Browser
import Element as E
import Element.Background
import Html exposing (Html)
import Html5.DragDrop
import Random
import TowerOfHanoi exposing (Pin(..), TowerOfHanoi, towerRepresentationOf)



-- Draggable test:
-- https://ellie-app.com/8QbM7KXrX6ka1


randomlyColoredDiscs : Bool
randomlyColoredDiscs =
    True


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }


type alias Model =
    { gameState : TowerOfHanoi
    , dragDrop : Html5.DragDrop.Model Pin Pin
    }


init : Model
init =
    { gameState = TowerOfHanoi.initial 5
    , dragDrop = Html5.DragDrop.init
    }


type Msg
    = DragDropMsg (Html5.DragDrop.Msg Pin Pin)


update : Msg -> Model -> Model
update msg model =
    case msg of
        DragDropMsg msg_ ->
            let
                ( model_, result ) =
                    Html5.DragDrop.update msg_ model.dragDrop
            in
            { model
                | dragDrop = model_
                , gameState =
                    case result of
                        Nothing ->
                            model.gameState

                        Just ( dragId, dropId, _ ) ->
                            case TowerOfHanoi.moveFromPinToPin model.gameState dragId dropId of
                                Nothing ->
                                    model.gameState

                                -- Show a nice error message
                                Just newGameState ->
                                    newGameState
            }



-- VIEW


draggable : (Html5.DragDrop.Msg Pin Pin -> Msg) -> Pin -> List (E.Attribute Msg)
draggable dndmsg id =
    List.map E.htmlAttribute <| Html5.DragDrop.draggable dndmsg id


droppable : (Html5.DragDrop.Msg Pin Pin -> Msg) -> Pin -> List (E.Attribute Msg)
droppable dndmsg id =
    List.map E.htmlAttribute <| Html5.DragDrop.droppable dndmsg id


view : Model -> Html Msg
view model =
    E.layout [] <|
        viewTowerOfHanoi model.gameState


viewTowerOfHanoi : TowerOfHanoi -> E.Element Msg
viewTowerOfHanoi toh =
    let
        pinMap =
            towerRepresentationOf toh
    in
    E.row
        [ E.centerX
        , E.centerY
        , E.spacing 150
        , E.height (E.px 200)
        ]
    <|
        List.map viewPin [ ( pinMap.pin1, Pin1 ), ( pinMap.pin2, Pin2 ), ( pinMap.pin3, Pin3 ) ]



-- Make the pins droppable- CHECK


viewPin : ( List Int, Pin ) -> E.Element Msg
viewPin ( listOfDiscSizesInOrder, pin ) =
    E.column
        ([ E.alignBottom
         , E.height E.fill
         , E.width (E.px 150)
         , E.spacing 5
         , E.behindContent
            (E.el
                [ E.centerX

                -- , E.alignBottom
                , E.height E.fill
                , E.width (E.px 20)
                , Element.Background.color (E.rgb255 255 180 60)
                ]
                E.none
            )
         ]
            ++ droppable DragDropMsg pin
        )
    <|
        viewDiscs pin listOfDiscSizesInOrder


viewDiscs : Pin -> List Int -> List (E.Element Msg)
viewDiscs pin sizes =
    case sizes of
        [] ->
            []

        x :: xs ->
            viewDraggableDisc x pin :: List.map viewDisc xs



--Make the last one draggable


randomColor : Random.Seed -> E.Color
randomColor seed0 =
    let
        ( r, seed1 ) =
            Random.step (Random.int 0 255) seed0

        ( g, seed2 ) =
            Random.step (Random.int 0 255) seed1

        ( b, _ ) =
            Random.step (Random.int 0 255) seed2
    in
    E.rgb255 r g b


discStyle size =
    [ E.alignBottom
    , E.centerX
    , E.height (E.px 30)
    , E.width (E.px <| 40 * size + 60)
    , Element.Background.color
        (if randomlyColoredDiscs then
            randomColor <| Random.initialSeed size

         else
            E.rgb255 100 100 100
        )
    ]


viewDisc : Int -> E.Element Msg
viewDisc size =
    E.el (discStyle size) E.none


viewDraggableDisc : Int -> Pin -> E.Element Msg
viewDraggableDisc size pin =
    E.el (discStyle size ++ draggable DragDropMsg pin) E.none
