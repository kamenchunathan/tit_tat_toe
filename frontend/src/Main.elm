module Main exposing (..)

import Browser
import Canvas as Canvas exposing (rect, shapes)
import Canvas.Settings exposing (fill)
import Color
import Html exposing (Html, div)
import Html.Attributes exposing (class)



---- MODEL ----


type alias Model =
    {}


init : ( Model, Cmd Msg )
init =
    ( {}, Cmd.none )



---- UPDATE ----


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update _ model =
    ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view _ =
    div [ class "flex flex-col items-center bg-gray-300 h-screen justify-center" ]
        [ Canvas.toHtml ( 300, 500 )
            [ class "bg-white" ]
            [ shapes
                [ fill Color.red ]
                [ rect ( 150, 150 ) 100 200 ]
            ]
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
