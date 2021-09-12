module Board exposing (..)

import Array exposing (Array)


type alias Board =
    Array (Array (Maybe Piece))


type alias Coord =
    ( Int, Int )


pieceAt : Coord -> Board -> Maybe Piece
pieceAt ( x, y ) board =
    case Maybe.andThen (Array.get y) (Array.get x board) of
        Just (Just piece) ->
            Just piece

        _ ->
            Nothing


hasPieceOf : Board -> Player -> Coord -> Bool
hasPieceOf board player coord =
    case pieceAt coord board of
        Just piece ->
            piece.player == player

        Nothing ->
            False


type Player
    = White
    | Black


otherPlayer : Player -> Player
otherPlayer p =
    case p of
        White ->
            Black

        Black ->
            White


playerColor : Player -> String
playerColor player =
    case player of
        White ->
            "#fff"

        Black ->
            "#000"


type Figure
    = Pawn
    | Rook
    | Knight
    | Bishop
    | King
    | Queen


type alias Piece =
    { figure : Figure, player : Player }


possibleMovements : Coord -> Piece -> Board -> List Coord
possibleMovements coord { figure, player } board =
    let
        movements =
            case figure of
                Pawn ->
                    pawnMovement coord player board

                Rook ->
                    rookMovement coord board

                Knight ->
                    knightMovement coord

                Bishop ->
                    bishopMovement coord board

                King ->
                    kingMovement coord board

                Queen ->
                    queenMovement coord board

        withoutTakingOwnPieces =
            List.filter
                (\c ->
                    pieceAt c board
                        |> Maybe.map ((/=) player << .player)
                        |> Maybe.withDefault True
                )
                movements
    in
    withoutTakingOwnPieces


mapBoardToList : (Coord -> Maybe Piece -> a) -> Board -> List a
mapBoardToList f =
    Array.map Array.toList
        >> Array.toList
        >> List.indexedMap (\y -> List.indexedMap (\x -> f ( x, y )))
        >> List.concat


inBounds : Coord -> Bool
inBounds ( x, y ) =
    x >= 0 && y >= 0 && x < 8 && y < 8


allMovementTargets : Board -> Player -> List Coord
allMovementTargets board player =
    board
        |> mapBoardToList
            (\coord occupant ->
                case occupant of
                    Just piece ->
                        if piece.player == player then
                            possibleMovements coord piece board

                        else
                            []

                    Nothing ->
                        []
            )
        |> List.concat


singletonIf : Bool -> a -> List a
singletonIf pred x =
    if pred then
        [ x ]

    else
        []


pawnMovement : Coord -> Player -> Board -> List Coord
pawnMovement ( x, y ) player board =
    case player of
        White ->
            (List.filter (\c -> pieceAt c board == Nothing) <|
                ( x, y + 1 )
                    :: singletonIf (y == 1) ( x, y + 2 )
            )
                ++ List.filter (hasPieceOf board Black)
                    [ ( x - 1, y + 1 )
                    , ( x + 1, y + 1 )
                    ]

        Black ->
            (List.filter (\c -> pieceAt c board == Nothing) <|
                ( x, y - 1 )
                    :: singletonIf (y == 6) ( x, y - 2 )
            )
                ++ List.filter (hasPieceOf board White)
                    [ ( x - 1, y - 1 )
                    , ( x + 1, y - 1 )
                    ]


knightMovement : Coord -> List Coord
knightMovement ( x, y ) =
    List.filter inBounds
        [ ( x + 1, y + 2 )
        , ( x - 1, y + 2 )
        , ( x + 1, y - 2 )
        , ( x - 1, y - 2 )
        , ( x + 2, y + 1 )
        , ( x - 2, y + 1 )
        , ( x + 2, y - 1 )
        , ( x - 2, y - 1 )
        ]


kingMovement : Coord -> Board -> List Coord
kingMovement ( x, y ) board =
    List.filter (\c -> pieceAt c board == Nothing && inBounds c)
        [ ( x + 1, y + 0 )
        , ( x - 1, y + 0 )
        , ( x + 1, y + 1 )
        , ( x - 1, y + 1 )
        , ( x + 1, y - 1 )
        , ( x - 1, y - 1 )
        , ( x + 0, y + 1 )
        , ( x - 0, y - 1 )
        ]


rookMovement : Coord -> Board -> List Coord
rookMovement coord board =
    List.concatMap (rayCastFields coord board)
        [ \( x, y ) -> ( x + 1, y )
        , \( x, y ) -> ( x - 1, y )
        , \( x, y ) -> ( x, y + 1 )
        , \( x, y ) -> ( x, y - 1 )
        ]


bishopMovement : Coord -> Board -> List Coord
bishopMovement coord board =
    List.concatMap (rayCastFields coord board)
        [ \( x, y ) -> ( x + 1, y + 1 )
        , \( x, y ) -> ( x - 1, y - 1 )
        , \( x, y ) -> ( x - 1, y + 1 )
        , \( x, y ) -> ( x + 1, y - 1 )
        ]


queenMovement : Coord -> Board -> List Coord
queenMovement coord board =
    bishopMovement coord board ++ rookMovement coord board


rayCastFields : Coord -> Board -> (Coord -> Coord) -> List Coord
rayCastFields origin board f =
    let
        coord =
            f origin
    in
    if inBounds coord then
        if pieceAt coord board == Nothing then
            coord :: rayCastFields coord board f

        else
            [ coord ]

    else
        []


toUnicode : Figure -> String
toUnicode figure =
    case figure of
        Pawn ->
            "♟︎"

        Rook ->
            "♜"

        Knight ->
            "♞"

        Bishop ->
            "♝"

        King ->
            "♚"

        Queen ->
            "♛"


generateColumn : Figure -> Figure -> Array (Maybe Piece)
generateColumn white black =
    Array.fromList
        [ Just <| Piece white White
        , Just <| Piece Pawn White
        , Nothing
        , Nothing
        , Nothing
        , Nothing
        , Just <| Piece Pawn Black
        , Just <| Piece black Black
        ]


initialBoard : Board
initialBoard =
    Array.fromList
        [ generateColumn Rook Rook
        , generateColumn Knight Knight
        , generateColumn Bishop Bishop
        , generateColumn King Queen
        , generateColumn Queen King
        , generateColumn Bishop Bishop
        , generateColumn Knight Knight
        , generateColumn Rook Rook
        ]
