port module Main exposing (..)

import Browser
import Canvas as Canvas exposing (Point, Renderable, circle, clear, lineTo, path, rect, shapes)
import Canvas.Settings exposing (fill)
import Canvas.Settings.Advanced exposing (rotate, scale, transform, translate)
import Color
import Graph as Graph exposing (Graph, fromNodeLabelsAndEdgePairs)
import Html exposing (Html, button, div, h2, text)
import Html.Attributes exposing (class)
import Html.Events exposing (on, onClick)
import Json.Decode as Decode
import Json.Encode as Encode exposing (Value)



---- MODEL ----


type alias Model =
    { socketUri : String
    , sockState : SocketState
    , board : Board
    , currentPlayer : Player
    , selectedBoardPos : Maybe BoardPosition
    , canvasDim : { width : Int, height : Int }
    }


type SocketReadyState
    = Connecting
    | Open
    | Closing
    | Closed


type alias SocketState =
    { readyState : SocketReadyState
    }


type alias Board =
    Graph BoardPosition ()


type Player
    = Player1
    | Player2


type Piece
    = Piece Player


type alias BoardPosition =
    { coord : ( Int, Int )
    , piece : Maybe Piece
    }


type alias MouseEvent =
    { offsetX : Int
    , offsetY : Int
    }


decodeClick : Decode.Decoder Msg
decodeClick =
    Decode.map
        (\{ offsetX, offsetY } -> CanvasClicked offsetX offsetY)
        (Decode.map2 MouseEvent
            (Decode.field "offsetX" Decode.int)
            (Decode.field "offsetY" Decode.int)
        )


makeBoard : Board
makeBoard =
    let
        boardPositions =
            List.map
                (\i ->
                    { coord = ( modBy 3 i, i // 3 )
                    , piece =
                        case i // 3 of
                            0 ->
                                Just <| Piece Player1

                            2 ->
                                Just <| Piece Player2

                            _ ->
                                Nothing
                    }
                )
                (List.range 0 8)

        edges =
            [ ( 0, 1 )
            , ( 0, 3 )
            , ( 0, 4 )
            , ( 1, 0 )
            , ( 1, 2 )
            , ( 1, 4 )
            , ( 2, 1 )
            , ( 2, 3 )
            , ( 2, 5 )
            , ( 3, 3 )
            , ( 3, 4 )
            , ( 3, 6 )
            , ( 4, 0 )
            , ( 4, 1 )
            , ( 4, 2 )
            , ( 4, 3 )
            , ( 4, 5 )
            , ( 4, 6 )
            , ( 4, 7 )
            , ( 4, 8 )
            , ( 4, 9 )
            , ( 5, 2 )
            , ( 5, 4 )
            , ( 5, 8 )
            , ( 6, 3 )
            , ( 6, 4 )
            , ( 6, 7 )
            , ( 7, 4 )
            , ( 7, 6 )
            , ( 7, 8 )
            , ( 8, 4 )
            , ( 8, 5 )
            , ( 8, 7 )
            ]
    in
    fromNodeLabelsAndEdgePairs boardPositions edges


init : ( Model, Cmd Msg )
init =
    ( { socketUri = ""
      , sockState = { readyState = Closed }
      , board = makeBoard
      , currentPlayer = Player1
      , selectedBoardPos = Nothing
      , canvasDim = { width = 600, height = 600 }
      }
    , Cmd.none
    )



---- UPDATE ----
-- message sent via port to websocket module


toPortMessage : SocketMessage -> Encode.Value
toPortMessage (OpenSocket uri) =
    Encode.object
        [ ( "uri", Encode.string uri )
        , ( "kind", Encode.string "open" )
        ]


toSocketMessage : Value -> SocketMessage
toSocketMessage _ =
    OpenSocket ""


type SocketMessage
    = OpenSocket String



-- | CloseSocket
--


type Msg
    = RecvSockMsg SocketMessage
    | SendSockMsg SocketMessage
    | CanvasClicked Int Int
    | NoOp


type Move
    = SelectBoardPos BoardPosition
    | DeselectBoardPos BoardPosition
    | MovePiece BoardPosition BoardPosition
    | IdentityMove


type MoveError
    = InvalidMove
    | UnconnectedPositions -- can only move to positions on the board that are connected by a path and immediately adjacent
    | TargetPosOccupied -- can only move to an empty board position


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RecvSockMsg _ ->
            ( model, Cmd.none )

        SendSockMsg ((OpenSocket _) as sockMsg) ->
            case model.sockState.readyState of
                Closed ->
                    ( model, sendMsg <| toPortMessage sockMsg )

                _ ->
                    ( model, Cmd.none )

        NoOp ->
            let
                _ =
                    Debug.log ".." "NoOp"
            in
            ( model, Cmd.none )

        CanvasClicked xCanvas yCanvas ->
            let
                -- distance touch has to be from a boardPosition (in hint coords) to be registered as a touch on a boardPosition
                sensitivity =
                    0.05

                xh =
                    toFloat xCanvas / toFloat model.canvasDim.width

                yh =
                    toFloat yCanvas / toFloat model.canvasDim.height

                -- Returns the board position if the area on canvas clicked is close enough (within distance
                -- specified by sensitivity) and Nothing if the point clicked on the canvas is not close enough
                -- to a valid board position
                touchedBoardPos : Maybe BoardPosition
                touchedBoardPos =
                    List.head
                        (List.filterMap
                            (\{ label } ->
                                let
                                    ( xc, yc ) =
                                        Tuple.mapBoth toFloat toFloat label.coord
                                in
                                if sqrt ((((xc * 0.4 + 0.1) - xh) ^ 2) + (((yc * 0.4 + 0.1) - yh) ^ 2)) <= sensitivity then
                                    Just label

                                else
                                    Nothing
                            )
                            (Graph.nodes model.board)
                        )

                move =
                    generateMove model.selectedBoardPos touchedBoardPos model.board

                ( newSelectedBPos, newBoard ) =
                    applyMove
                        (Result.withDefault IdentityMove move)
                        model.selectedBoardPos
                        model.board

                _ =
                    Debug.log "Touched position" move

                -- Make a move using the currently selected position and the currently clicked position
            in
            ( { model
                | selectedBoardPos = newSelectedBPos
                , board = newBoard
              }
            , Cmd.none
            )


posNodeId : BoardPosition -> Int
posNodeId { coord } =
    let
        ( x, y ) =
            coord
    in
    y * 3 + x


generateMove : Maybe BoardPosition -> Maybe BoardPosition -> Board -> Result MoveError Move
generateMove currentlySelectedBoardPos touchedBoardPos board =
    case ( currentlySelectedBoardPos, touchedBoardPos ) of
        -- only occupied with a piece can be selected
        ( Nothing, Just tBPos ) ->
            if tBPos.piece /= Nothing then
                Ok (SelectBoardPos tBPos)

            else
                Ok IdentityMove

        ( Just pos1, Just pos2 ) ->
            -- if same position is clicked deselect currently selected board position
            if pos1 == pos2 then
                Ok (DeselectBoardPos pos1)

            else
                -- Check if the move is valid
                Ok (MovePiece pos1 pos2)
                    |> Result.andThen
                        (\move ->
                            if checkConnectedPos pos1 pos2 board then
                                Ok move

                            else
                                Err UnconnectedPositions
                        )
                    |> Result.andThen
                        (\move ->
                            if checkTargetPosEmpty pos2 then
                                Ok move

                            else
                                Err TargetPosOccupied
                        )

        ( _, _ ) ->
            Err InvalidMove


checkConnectedPos : BoardPosition -> BoardPosition -> Board -> Bool
checkConnectedPos pos1 pos2 board =
    Graph.fold
        (\ctx acc ->
            if ctx.node.label == pos1 then
                List.any (\x -> x == posNodeId pos2) (Graph.alongOutgoingEdges ctx)

            else
                acc
        )
        False
        board


checkTargetPosEmpty : BoardPosition -> Bool
checkTargetPosEmpty bPos =
    case bPos.piece of
        Nothing ->
            True

        Just _ ->
            False


applyMove : Move -> Maybe BoardPosition -> Board -> ( Maybe BoardPosition, Board )
applyMove m currSelectedBoardPos board =
    case m of
        IdentityMove ->
            ( currSelectedBoardPos, board )

        SelectBoardPos bPos ->
            ( Just bPos, board )

        DeselectBoardPos _ ->
            ( Nothing, board )

        MovePiece pos1 pos2 ->
            let
                p =
                    Graph.get (posNodeId pos1) board
                        |> Maybe.andThen (\x -> x.node.label.piece)

                newBoard =
                    Graph.mapNodes
                        (\n ->
                            if n == pos1 then
                                { n | piece = Nothing }

                            else if n == pos2 then
                                { n | piece = p }

                            else
                                n
                        )
                        board
            in
            ( Nothing
            , newBoard
            )



---- VIEW ----


view : Model -> Html Msg
view model =
    div [ class "flex flex-col items-center bg-gray-300 h-screen justify-center" ]
        [ div [ class "p-5" ]
            [ h2 [] [ text "Connect to \"ws://localhost:8001\"" ]
            , button
                [ onClick NoOp

                {- <| SendSockMsg (OpenSocket "ws://localhost:8001") -}
                , class "bg-blue-500 text-white p-2 m-2 rounded-lg drop-shadow-lg"
                ]
                [ text "Print" ]
            ]
        , viewCanvas model
        ]


viewCanvas : Model -> Html Msg
viewCanvas model =
    let
        -- _ =
        --     Debug.log ".." model.canvasDim
        --
        lineWidth =
            x_hint 0.005

        lineColor =
            Color.darkBlue

        fromRelativePos : Float -> Float -> Point
        fromRelativePos x y =
            ( x_hint x, y_hint y )

        y_hint : Float -> Float
        y_hint f =
            f * toFloat model.canvasDim.width

        x_hint : Float -> Float
        x_hint f =
            f * toFloat model.canvasDim.width

        hypotenuseLength : Float -> Float -> Float
        hypotenuseLength x y =
            sqrt (x * x + y * y)

        playerPiece : Point -> Bool -> Renderable
        playerPiece ( x, y ) shouldRotate =
            shapes
                [ transform
                    ([ translate x y, scale 0.05 0.05 ]
                        ++ (if shouldRotate then
                                [ rotate (degrees 180) ]

                            else
                                []
                           )
                    )
                , fill
                    (if shouldRotate then
                        Color.lightPurple

                     else
                        Color.lightOrange
                    )
                ]
                [ path (fromRelativePos 0 -0.5)
                    [ lineTo (fromRelativePos 0.5 0.5)
                    , lineTo (fromRelativePos -0.5 0.5)
                    ]
                ]

        playerPieces : List Renderable
        playerPieces =
            List.filterMap
                (\node ->
                    let
                        -- _ =
                        --     Debug.log "..." ( x, y, boardPos )
                        --
                        ( x, y ) =
                            node.label.coord
                    in
                    Maybe.map
                        (\(Piece player) ->
                            playerPiece (fromRelativePos ((toFloat x * 0.4) + 0.1) ((toFloat y * 0.4) + 0.1))
                                (case player of
                                    Player1 ->
                                        True

                                    Player2 ->
                                        False
                                )
                        )
                        node.label.piece
                )
                (Graph.nodes model.board)
    in
    Canvas.toHtml ( model.canvasDim.width, model.canvasDim.height )
        [ class "bg-white", on "click" decodeClick ]
        ([ -- clear
           clear ( 0, 0 ) (toFloat model.canvasDim.width) (toFloat model.canvasDim.height)

         -- highlight the currently selected piece
         -- done at the beginning so it can be drawn over
         , shapes [ fill <| Color.rgb255 247 238 136 ]
            (case model.selectedBoardPos of
                Just bpos ->
                    [ circle
                        (Tuple.mapBoth
                            (\x -> toFloat x * 0.4 + 0.1 |> x_hint)
                            (\y -> toFloat y * 0.4 + 0.1 |> y_hint)
                            bpos.coord
                        )
                        (x_hint 0.05)
                    ]

                Nothing ->
                    []
            )

         -- Draw the board (lines showing where you can move)
         , shapes
            [ fill lineColor ]
            [ -- horizontal lines
              rect (fromRelativePos 0.1 0.1) (x_hint 0.8) lineWidth
            , rect (fromRelativePos 0.1 (0.5 + 0.0025)) (x_hint 0.8) lineWidth
            , rect (fromRelativePos 0.1 0.9) (x_hint 0.8 + lineWidth) lineWidth

            -- vertical lines
            , rect (fromRelativePos 0.1 0.1) lineWidth (x_hint 0.8)
            , rect (fromRelativePos (0.5 - 0.0025) 0.1) lineWidth (x_hint 0.8)
            , rect (fromRelativePos 0.9 0.1) lineWidth (x_hint 0.8)
            ]

         -- diagonal lines
         , shapes
            [ transform <|
                [ translate (x_hint 0.1) (y_hint 0.1)
                , rotate (degrees 45)
                ]
            , fill lineColor
            ]
            [ rect (fromRelativePos 0 0)
                (hypotenuseLength (x_hint 0.8) (y_hint 0.8))
                lineWidth
            ]
         , shapes
            [ transform <|
                [ translate (x_hint 0.1) (y_hint 0.9)
                , rotate (degrees -45)
                ]
            , fill lineColor
            ]
            [ rect (fromRelativePos 0 0)
                (hypotenuseLength (x_hint 0.8) (y_hint 0.8))
                lineWidth
            ]
         ]
            ++ playerPieces
        )



---- SUBSCRIPTIONS ----


port sendMsg : Value -> Cmd msg


port recvMsg : (Value -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions _ =
    recvMsg (toSocketMessage >> RecvSockMsg)



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = subscriptions
        }
