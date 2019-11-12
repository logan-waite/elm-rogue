module Main exposing (main)

import Board exposing (Msg(..), updateBoard)
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


type alias Position =
    { x : Int
    , y : Int
    }


type Direction
    = Up
    | Down
    | Left
    | Right


type alias Model =
    { board : Board.Model
    , currentPosition : Position
    }


initialModel : Model
initialModel =
    { board = Board.initialModel
    , currentPosition = { x = 0, y = 0 }
    }



-- UPDATE


type Msg
    = NewGame
    | Move Direction
    | UpdateBoard Board.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewGame ->
            update (UpdateBoard Board.CreateNewBoard) initialModel

        UpdateBoard boardMsg ->
            convertBoardMsg model (Board.update boardMsg model.board)

        Move direction ->
            let
                boardLimit =
                    model.board.size // 2

                currentPosition =
                    model.currentPosition

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
            { model | currentPosition = updatedPosition }
                |> update (UpdateBoard (Board.AddTile <| convertPosition updatedPosition))


convertBoardMsg : Model -> ( Board.Model, Cmd Board.Msg ) -> ( Model, Cmd Msg )
convertBoardMsg model ( board, cmd ) =
    ( { model | board = board }
    , Cmd.map UpdateBoard cmd
    )


convertPosition : Position -> ( Int, Int )
convertPosition position =
    Tuple.pair position.x position.y



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
                [ column [ alignTop, spacing 2 ]
                    [ Input.button [ padding 10, Background.color (rgb255 255 255 255) ] { onPress = Just NewGame, label = text "New Game" }
                    , Input.button [ padding 10, Background.color (rgb255 255 255 255) ] { onPress = Just (Move Up), label = text "Add Tile Up" }
                    , Input.button [ padding 10, Background.color (rgb255 255 255 255) ] { onPress = Just (Move Right), label = text "Add Tile Right" }
                    , Input.button [ padding 10, Background.color (rgb255 255 255 255) ] { onPress = Just (Move Down), label = text "Add Tile Down" }
                    , Input.button [ padding 10, Background.color (rgb255 255 255 255) ] { onPress = Just (Move Left), label = text "Add Tile Left" }
                    ]
                , wrappedRow [ width fill, height fill, Background.color (rgb255 100 0 0) ]
                    [ renderedBoard model.board ]
                ]
        )


renderedBoard : Board.Model -> Element Msg
renderedBoard board =
    Board.view board
        |> Element.map UpdateBoard
