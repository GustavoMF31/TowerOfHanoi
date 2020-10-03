module TowerOfHanoi exposing (Pin(..), PinsMap, TowerOfHanoi, initial, moveFromPinToPin, towerRepresentationOf)

import Array exposing (Array)
import List.Extra


type Pin
    = Pin1
    | Pin2
    | Pin3



--  Maps the size of the disc to the pin it is in


type TowerOfHanoi
    = TowerOfHanoi (Array Pin)



-- A more visualization friendly representation of a game state


type alias PinsMap =
    { pin1 : List Int
    , pin2 : List Int
    , pin3 : List Int
    }


initial : Int -> TowerOfHanoi
initial discCount =
    TowerOfHanoi <| Array.repeat discCount Pin1



-- The resulting representation should be in order


towerRepresentationOf : TowerOfHanoi -> PinsMap
towerRepresentationOf (TowerOfHanoi array) =
    Debug.log "Folding tower representation of" <| List.foldl groupDiscs { pin1 = [], pin2 = [], pin3 = [] } (Array.toIndexedList array)


groupDiscs : ( Int, Pin ) -> PinsMap -> PinsMap
groupDiscs ( discSize, pin ) currentGrouping =
    case pin of
        Pin1 ->
            { currentGrouping | pin1 = currentGrouping.pin1 ++ [ discSize ] }

        Pin2 ->
            { currentGrouping | pin2 = currentGrouping.pin2 ++ [ discSize ] }

        Pin3 ->
            { currentGrouping | pin3 = currentGrouping.pin3 ++ [ discSize ] }


smallestDiscInPin : Array Pin -> Pin -> Maybe Int
smallestDiscInPin sizeToPin pin =
    sizeToPin
        |> Array.toIndexedList
        |> List.filter (\( _, discPin ) -> discPin == pin)
        |> List.Extra.minimumBy Tuple.first
        -- |> List.Extra.foldl1
        --     (\currentBiggest newDisc ->
        --         if Tuple.first currentBiggest > Tuple.first newDisc then
        --             currentBiggest
        --         else
        --             newDisc
        --     )
        |> Maybe.map Tuple.first


isValidMove : Array Pin -> Pin -> Pin -> Bool
isValidMove game from to =
    case smallestDiscInPin game from of
        Nothing ->
            False

        Just originDiscSize ->
            case smallestDiscInPin game to of
                Nothing ->
                    True

                Just destinationDiscSize ->
                    -- If they are equal it means i'm placing a disc on the pin it was already at
                    -- I'll consider that a valid move, even if it doesn't change the game's state
                    destinationDiscSize >= originDiscSize


moveFromPinToPin : TowerOfHanoi -> Pin -> Pin -> Maybe TowerOfHanoi
moveFromPinToPin (TowerOfHanoi game) from to =
    if isValidMove game from to then
        case smallestDiscInPin game from of
            Nothing ->
                Nothing

            Just discSize ->
                Just <| TowerOfHanoi <| Array.set discSize to game

    else
        Nothing
