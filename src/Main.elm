module Main exposing (main)

import Browser
import Element exposing (Element, alignTop, centerX, centerY, column, el, fill, height, layout, none, padding, px, rgb255, row, spacing, text, width, wrappedRow)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html
import List.Extra
import Random


main : Program () Model Msg
main =
    Browser.element
        { init = \flags -> ( initialModel, Cmd.none )
        , subscriptions = \_ -> Sub.none
        , view = view
        , update = update
        }


type alias Model =
    { boardState : BoardState
    , currentPosition : Position
    }


type alias Position =
    { x : Int
    , y : Int
    }


initialModel : Model
initialModel =
    { boardState = initialBoardState
    , currentPosition = { x = 0, y = 0 }
    }



-- UPDATE


type Msg
    = NewGame
    | AddTile Direction
    | InsertTile Tile


type Direction
    = Up
    | Down
    | Left
    | Right


startGameTiles : List ( Maybe Tile, SlotPosition )
startGameTiles =
    [ ( Just Space, ( 0, 0 ) )
    ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewGame ->
            ( { model | boardState = updateBoard startGameTiles model.boardState }, Cmd.none )

        AddTile direction ->
            let
                currentPosition =
                    model.currentPosition

                boardLimit =
                    model.boardState.size // 2

                updatedPosition =
                    case direction of
                        Up ->
                            if currentPosition.y /= boardLimit then
                                { currentPosition | y = model.currentPosition.y + 1 }

                            else
                                currentPosition

                        Right ->
                            if currentPosition.x /= boardLimit then
                                { currentPosition | x = model.currentPosition.x + 1 }

                            else
                                currentPosition

                        Down ->
                            if currentPosition.y /= -boardLimit then
                                { currentPosition | y = model.currentPosition.y - 1 }

                            else
                                currentPosition

                        Left ->
                            if currentPosition.x /= -boardLimit then
                                { currentPosition | x = model.currentPosition.x - 1 }

                            else
                                currentPosition
            in
            ( { model | currentPosition = updatedPosition }, generateTile )

        InsertTile tile ->
            let
                updatedBoardState =
                    updateBoard [ ( Just tile, convertPositionToSlotPosition model.currentPosition ) ] model.boardState
            in
            ( { model | boardState = updatedBoardState }, Cmd.none )


convertPositionToSlotPosition : Position -> SlotPosition
convertPositionToSlotPosition position =
    Tuple.pair position.x position.y


updateBoard : List ( Maybe Tile, SlotPosition ) -> BoardState -> BoardState
updateBoard tiles boardState =
    case tiles of
        tile :: restTiles ->
            updateTileAtPosition
                boardState
                (Tuple.second tile)
                (Tuple.first tile)
                |> updateBoard restTiles

        [] ->
            boardState


updateTileAtPosition : BoardState -> SlotPosition -> Maybe Tile -> BoardState
updateTileAtPosition boardState position tile =
    let
        x =
            boardState.size
                // 2
                + Tuple.first position

        y =
            boardState.size
                // 2
                + -(Tuple.second position)

        rowToUpdate : List Slot
        rowToUpdate =
            case List.Extra.getAt y boardState.slots of
                Just row ->
                    row

                Nothing ->
                    []

        slotToUpdate : Slot
        slotToUpdate =
            case List.Extra.getAt x rowToUpdate of
                Just slot ->
                    slot

                Nothing ->
                    { tile = Nothing, position = ( x, y ) }

        updatedSlots =
            List.Extra.setAt
                y
                (List.Extra.setAt x { slotToUpdate | tile = tile } rowToUpdate)
                boardState.slots
    in
    { boardState | slots = updatedSlots }



-- VIEW


view : Model -> Html.Html Msg
view model =
    layout
        []
        (el
            [ Element.alignTop
            , Element.centerX
            , width Element.fill
            , height Element.fill
            , Background.color (rgb255 0 100 0)
            ]
         <|
            row
                [ height fill
                , width fill
                ]
                [ column [ alignTop ]
                    [ Input.button [ padding 10 ] { onPress = Just NewGame, label = text "New Game" }
                    , Input.button [ padding 10 ] { onPress = Just (AddTile Up), label = text "Add Tile Up" }
                    , Input.button [ padding 10 ] { onPress = Just (AddTile Right), label = text "Add Tile Right" }
                    , Input.button [ padding 10 ] { onPress = Just (AddTile Down), label = text "Add Tile Down" }
                    , Input.button [ padding 10 ] { onPress = Just (AddTile Left), label = text "Add Tile Left" }
                    ]
                , wrappedRow [ width fill, height fill, Background.color (rgb255 100 0 0) ]
                    [ renderBoard model.boardState ]
                ]
        )



-- BOARD RENDER


renderBoard : BoardState -> Element msg
renderBoard boardState =
    el
        [ centerX
        , centerY
        ]
    <|
        renderBoardSlots boardState


renderBoardSlots : BoardState -> Element msg
renderBoardSlots boardState =
    column
        [ spacing 1 ]
    <|
        List.map renderRow boardState.slots


renderRow : List Slot -> Element msg
renderRow rowSlots =
    row
        [ spacing 1 ]
    <|
        List.map (\slot -> renderSlot slot) rowSlots



-- SLOTS


type alias SlotPosition =
    ( Int, Int )


type alias Slot =
    { tile : Maybe Tile
    , position : SlotPosition
    }


type alias SlotGrid =
    List (List Slot)


type alias BoardState =
    { slots : SlotGrid
    , size : Int
    }


initialSlot : Slot
initialSlot =
    { tile = Nothing
    , position = ( 0, 0 )
    }


initialBoardState : BoardState
initialBoardState =
    let
        size : Int
        size =
            5

        slots : SlotGrid
        slots =
            List.repeat size (List.repeat size initialSlot)
    in
    { slots = slots, size = size }
        |> setSlotCoordinates


setSlotCoordinates : BoardState -> BoardState
setSlotCoordinates boardState =
    let
        updatedSlots =
            boardState.slots
                |> List.indexedMap (setPosition boardState.size)
    in
    { boardState | slots = updatedSlots }


setPosition boardSize yIndex row =
    let
        y =
            boardSize // 2 - yIndex

        setCoords xIndex slot =
            let
                x =
                    -(boardSize // 2 - xIndex)
            in
            { slot | position = ( x, y ) }
    in
    List.indexedMap setCoords row


getYCoords : Int -> Int -> List Slot -> ( Int, List Slot )
getYCoords boardSize index slotRow =
    let
        yCoord =
            boardSize // 2 - index
    in
    Tuple.pair yCoord slotRow


renderSlot : Slot -> Element msg
renderSlot slot =
    el
        [ width (px 50)
        , height (px 50)
        , Background.color (rgb255 100 100 100)
        , Font.size 12
        , Font.center
        , centerX
        , centerY
        ]
    <|
        case slot.tile of
            Nothing ->
                text
                    ("("
                        ++ String.fromInt (Tuple.first slot.position)
                        ++ ", "
                        ++ String.fromInt (Tuple.second slot.position)
                        ++ ")"
                    )

            Just tile ->
                renderTile tile



-- TILES


type Tile
    = Space -- No Border
    | Wall -- One Border
    | Corner -- Two Borders next to each other
    | Hall -- Two Borders opposite each other
    | DeadEnd -- Three Borders


type alias TileBorder =
    { top : Int
    , right : Int
    , bottom : Int
    , left : Int
    }


defaultTileBorder : TileBorder
defaultTileBorder =
    { top = 0, right = 0, bottom = 0, left = 0 }


type alias BorderWidth =
    Int


borderWidth : BorderWidth
borderWidth =
    5


renderTile : Tile -> Element msg
renderTile tile =
    case tile of
        Wall ->
            basicTile
                (Border.widthEach
                    { defaultTileBorder | top = borderWidth }
                )

        Corner ->
            basicTile
                (Border.widthEach
                    { defaultTileBorder | top = borderWidth, right = borderWidth }
                )

        Hall ->
            basicTile
                (Border.widthEach
                    { defaultTileBorder | top = borderWidth, bottom = borderWidth }
                )

        DeadEnd ->
            basicTile
                (Border.widthEach
                    { defaultTileBorder | top = borderWidth, right = borderWidth, bottom = borderWidth }
                )

        Space ->
            basicTile
                (Border.widthEach defaultTileBorder)


basicTile : Element.Attribute msg -> Element msg
basicTile border =
    Element.el
        [ Background.color (rgb255 200 200 200)
        , Border.solid
        , Border.color (rgb255 255 255 255)
        , border
        , centerX
        , centerY
        , height (px 50)
        , width (px 50)
        ]
        none


tileGenerator : Random.Generator Tile
tileGenerator =
    Random.uniform Space [ Wall, Corner, Hall, DeadEnd ]


generateTile : Cmd Msg
generateTile =
    Random.generate InsertTile tileGenerator
