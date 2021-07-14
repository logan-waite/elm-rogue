module Board exposing (..)

import Element exposing (Element, centerX, centerY, column, el, height, none, px, rgb255, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import List.Extra
import Random



-- MODEL


type alias Model =
    { slots : SlotGrid
    , size : Int
    }


initialModel : Model
initialModel =
    let
        size : Int
        size =
            9

        slots : SlotGrid
        slots =
            List.repeat size (List.repeat size initialSlot)
    in
    { slots = slots, size = size }
        |> setSlotCoordinates



-- UPDATE


type Msg
    = CreateNewBoard
    | AddTile Position
    | InsertTile Position Tile
    | GenerateTile Position TileType


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CreateNewBoard ->
            let
                startGameTiles : List ( Maybe Tile, Position )
                startGameTiles =
                    [ ( Just { tileType = Wall, rotation = None }, ( 0, 0 ) )
                    ]
            in
            ( updateBoard startGameTiles initialModel, Cmd.none )

        AddTile position ->
            ( model, generateTile position )

        InsertTile position tile ->
            ( updateBoard [ ( Just tile, position ) ] model, Cmd.none )

        GenerateTile position tileType ->
            let
                generatedTile =
                    { tileType = tileType, rotation = Once }
            in
            update (InsertTile position generatedTile) model



-- VIEW


view : Model -> Element Msg
view model =
    renderBoard model



-- INITIAL


setSlotCoordinates : Model -> Model
setSlotCoordinates model =
    let
        updatedSlots =
            model.slots
                |> List.indexedMap (setPosition model.size)
    in
    { model | slots = updatedSlots }


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



-- BOARD
-- Data


updateBoard : List ( Maybe Tile, Position ) -> Model -> Model
updateBoard tiles model =
    case tiles of
        tile :: restTiles ->
            updateTileAtPosition
                model
                (Tuple.second tile)
                (Tuple.first tile)
                |> updateBoard restTiles

        [] ->
            model


updateTileAtPosition : Model -> Position -> Maybe Tile -> Model
updateTileAtPosition model position tile =
    let
        x =
            model.size
                // 2
                + Tuple.first position

        y =
            model.size
                // 2
                + -(Tuple.second position)

        rowToUpdate : List Slot
        rowToUpdate =
            case List.Extra.getAt y model.slots of
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
                model.slots
    in
    { model | slots = updatedSlots }



-- Render


renderBoard : Model -> Element msg
renderBoard model =
    el
        [ centerX
        , centerY
        ]
    <|
        renderBoardSlots model


renderBoardSlots : Model -> Element msg
renderBoardSlots model =
    column
        [ spacing 1 ]
    <|
        List.map renderRow model.slots


renderRow : List Slot -> Element msg
renderRow rowSlots =
    row
        [ spacing 1 ]
    <|
        List.map (\slot -> renderSlot slot) rowSlots



-- SLOTS


type alias SlotGrid =
    List (List Slot)


type alias Slot =
    { tile : Maybe Tile
    , position : Position
    }


type alias Position =
    ( Int, Int )


initialSlot : Slot
initialSlot =
    { tile = Nothing
    , position = ( 0, 0 )
    }


slotSize : Int
slotSize =
    65


renderSlot : Slot -> Element msg
renderSlot slot =
    el
        [ width (px slotSize)
        , height (px slotSize)
        ]
    <|
        case slot.tile of
            Nothing ->
                none

            Just tile ->
                renderTile tile



-- TILES


type alias Tile =
    { tileType : TileType

    -- , players : List (Maybe Player)
    , rotation : Rotation
    }


type TileType
    = Space -- No Border
    | Wall -- One Border
    | Corner -- Two Borders next to each other
    | Hall -- Two Borders opposite each other
    | DeadEnd -- Three Borders


type Rotation
    = None
    | Once
    | Twice
    | Thrice


type alias TileBorder =
    { top : Int
    , right : Int
    , bottom : Int
    , left : Int
    }


type alias BorderWidth =
    Int



-- Data


generateTile : Position -> Cmd Msg
generateTile position =
    Random.generate (GenerateTile position) tileGenerator


tileGenerator : Random.Generator TileType
tileGenerator =
    Random.uniform Space [ Wall, Corner, Hall, DeadEnd ]



-- Render


renderTile : Tile -> Element msg
renderTile tile =
    case tile.tileType of
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


defaultTileBorder : TileBorder
defaultTileBorder =
    { top = 0, right = 0, bottom = 0, left = 0 }


basicTile : Element.Attribute msg -> Element msg
basicTile border =
    Element.el
        [ Background.color (rgb255 200 200 200)
        , Border.solid
        , Border.color (rgb255 255 255 255)
        , border
        , centerX
        , centerY
        , height (px slotSize)
        , width (px slotSize)
        ]
        none


borderWidth : BorderWidth
borderWidth =
    5
