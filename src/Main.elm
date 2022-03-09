module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (type_)
import Html.Events exposing (onInput)


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { money : Int
    , cardA : Int
    , cardB : Int
    , moneyBet : Int
    , error : Maybe String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { money = 100, cardA = 3, cardB = 13, moneyBet = 0, error = Maybe.Nothing }, Cmd.none )


type Msg
    = BetMoney Int
    | UpdateBetValue String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        BetMoney bet ->
            ( { model | moneyBet = bet }, Cmd.none )

        UpdateBetValue value ->
            case String.toInt value of
                Just newValue ->
                    if newValue > model.money then
                        ( { model | error = Just "You cannot bet more than you have", moneyBet = model.money }, Cmd.none )

                    else
                        ( { model | moneyBet = newValue, error = Nothing }, Cmd.none )

                Nothing ->
                    ( { model | error = Just "Wrong input for bet" }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Model -> Html Msg
view model =
    div []
        [ p [] [ text "ACEY DUCEY CARD GAME" ]
        , p [] [ text "CREATIVE COMPUTING  MORRISTOWN, NEW JERSEY" ]
        , p [] [ text "ACEY-DUCEY IS PLAYED IN THE FOLLOWING MANNER " ]
        , p [] [ text "THE DEALER (COMPUTER) DEALS TWO CARDS FACE UP" ]
        , p [] [ text "YOU HAVE AN OPTION TO BET OR NOT BET DEPENDING" ]
        , p [] [ text "ON WHETHER OR NOT YOU FEEL THE CARD WILL HAVE" ]
        , p [] [ text "A VALUE BETWEEN THE FIRST TWO." ]
        , p [] [ text "IF YOU DO NOT WANT TO BET, INPUT A 0" ]
        , p [] [ text ("Currently you have " ++ String.fromInt model.money ++ " in your pocket") ]
        , p [] [ text ("Card A " ++ String.fromInt model.cardA) ]
        , p [] [ text ("Card B " ++ String.fromInt model.cardB) ]
        , p [] [ text ("Your current bet " ++ String.fromInt model.moneyBet) ]
        , input [ type_ "number", Html.Attributes.max (String.fromInt model.money), Html.Attributes.min "0", onInput UpdateBetValue ] []
        , button [] [ text "Play" ]
        , showError model.error
        ]


showError : Maybe String -> Html Msg
showError value =
    case value of
        Just string ->
            p [] [ text string ]

        Nothing ->
            div [] []
