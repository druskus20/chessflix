module Main exposing (..)


import Array exposing (Array)
import Board exposing (..)
import Browser
import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, value)
import Html.Styled.Events exposing (on, onClick, targetValue)
import List exposing (filterMap)
import Styles



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view >> Html.Styled.toUnstyled }



-- MODEL


type alias Model =
    { board : Board
    , selected : Maybe Coord
    , turn : Player
    , takenPieces : List Piece
    }


init : Model
init =
    { board = initialBoard
    , selected = Nothing
    , turn = White
    , takenPieces = []
    }



-- UPDATE


type Msg
    = FieldClicked Coord


update : Msg -> Model -> Model
update msg model =
    case msg of
        FieldClicked coord ->
            case model.selected of
                Just curSelected ->
                    if curSelected == coord then
                        { model | selected = Nothing }

                    else
                        case pieceAt curSelected model.board of
                            Just piece ->
                                let
                                    movements =
                                        possibleMovements curSelected piece model.board
                                in
                                if List.member coord movements then
                                    let
                                        ( takenPiece, newBoard ) =
                                            moveOn model.board curSelected coord
                                    in
                                    { model
                                        | selected = Nothing
                                        , board = newBoard
                                        , turn = otherPlayer model.turn
                                        , takenPieces =
                                            case takenPiece of
                                                Just p ->
                                                    p :: model.takenPieces

                                                Nothing ->
                                                    model.takenPieces
                                    }

                                else
                                    model

                            -- this should actually never be possible
                            Nothing ->
                                model

                Nothing ->
                    case pieceAt coord model.board of
                        Just { player } ->
                            if player == model.turn then
                                { model | selected = Just coord }

                            else
                                model

                        Nothing ->
                            model


moveOn : Board -> Coord -> Coord -> ( Maybe Piece, Board )
moveOn board start end =
    let
        afterRemoval =
            boardUpdate (\_ -> Nothing) start board
    in
    ( pieceAt end board, boardUpdate (\_ -> pieceAt start board) end afterRemoval )


boardUpdate : (Maybe Piece -> Maybe Piece) -> Coord -> Board -> Board
boardUpdate f ( x, y ) =
    arrayUpdate (arrayUpdate f y) x


arrayUpdate : (a -> a) -> Int -> Array a -> Array a
arrayUpdate f idx xs =
    case Array.get idx xs of
        Just elem ->
            Array.set idx (f elem) xs

        Nothing ->
            xs



-- VIEW


viewField : Coord -> Maybe Piece -> Bool -> Bool -> Html Msg
viewField coord piece movementIndicator isLight =
    let
        bgColor =
            if isLight then
                hex "#8ec07c"

            else
                hex "#504945"
    in
    case piece of
        Just { figure, player } ->
            div
                [ onClick (FieldClicked coord)
                , Styles.fieldStyle
                , css
                    [ backgroundColor bgColor
                    , color <| hex <| playerColor player
                    ]
                ]
                [ text <|
                    if movementIndicator then
                        "(" ++ toUnicode figure ++ ")"

                    else
                        toUnicode figure
                ]

        Nothing ->
            div
                [ Styles.fieldStyle
                , onClick (FieldClicked coord)
                , css
                    [ backgroundColor bgColor
                    , color (hex "#ff0000")
                    , fontSize (px 36)
                    ]
                ]
                [ text <|
                    if movementIndicator then
                        "·"

                    else
                        ""
                ]


view : Model -> Html Msg
view model =
    let
        movementOfSelected =
            case Debug.log "selected" model.selected of
                Just coord ->
                    Maybe.map
                        (\piece -> possibleMovements coord piece model.board)
                        (Debug.log "piece" <| pieceAt coord model.board)

                _ ->
                    Nothing

        isPossibleMovement coord =
            Maybe.withDefault False (Maybe.map (List.member coord) movementOfSelected)
    in
    div []
        [ viewActivePlayer model.turn
        , viewPieceList model.takenPieces
        , div [ Styles.boardStyle ] <|
            List.indexedMap
                (\x col ->
                    div [ Styles.rowStyle ] <|
                        List.indexedMap
                            (\y piece ->
                                viewField ( x, y )
                                    piece
                                    (isPossibleMovement ( x, y ))
                                    (modBy 2 (x + modBy 2 y) == 0)
                            )
                            col
                )
                (List.map Array.toList (Array.toList model.board))
        ]


viewPieceList : List Piece -> Html msg
viewPieceList pieces =
    let
        listPieces ps player =
            ps
                |> List.filter (.player >> (==) player)
                |> List.map (.figure >> toUnicode >> text)
    in
    div []
        [ div []
            [ text "white: "
            , span [] (listPieces pieces White)
            ]
        , div []
            [ text "black: "
            , span [] (listPieces pieces Black)
            ]
        ]


viewActivePlayer : Player -> Html msg
viewActivePlayer player =
    case player of
        White ->
            h1 [] [ text "white" ]

        Black ->
            h1 [] [ text "black" ]
