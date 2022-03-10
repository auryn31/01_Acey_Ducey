module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (style, type_, value)
import Html.Events exposing (onInput)
import Random


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { money : Int
    , currentGame : Game
    , lastGame : Maybe Game
    , moneyBet : Int
    , error : Maybe String
    }


type alias Game =
    { cardA : Maybe Int
    , cardB : Maybe Int
    , cardC : Maybe Int
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { money = 100, currentGame = { cardA = Nothing, cardB = Nothing, cardC = Nothing }, lastGame = Nothing, moneyBet = 0, error = Nothing }, Random.generate NewCard newCard )


type Msg
    = BetMoney Int
    | UpdateBetValue String
    | NewCard Int
    | NewCardC Int
    | Play
    | NewGame


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

        NewCard card ->
            case model.currentGame.cardA of
                Nothing ->
                    let
                        currentGame =
                            model.currentGame
                    in
                    if card > 13 then
                        ( model, Random.generate NewCard newCard )

                    else
                        ( { model | currentGame = { currentGame | cardA = Just card } }, Random.generate NewCard newCard )

                Just cardA ->
                    let
                        currentGame =
                            model.currentGame
                    in
                    if card <= cardA then
                        ( { model | currentGame = { currentGame | cardA = Just card } }, Random.generate NewCard newCard )

                    else
                        ( { model | currentGame = { currentGame | cardB = Just card } }, Cmd.none )

        Play ->
            ( model, Random.generate NewCardC newCard )

        NewCardC card ->
            ( calculateNewState card model, Random.generate NewCard newCard )

        NewGame ->
            init ()


calculateNewState : Int -> Model -> Model
calculateNewState cardC model =
    case model.currentGame.cardA of
        Just cardA ->
            case model.currentGame.cardB of
                Just cardB ->
                    let
                        currentGame =
                            model.currentGame

                        lastGame =
                            model.lastGame
                    in
                    if cardA < cardC && cardB > cardC then
                        { model | money = model.money + model.moneyBet, currentGame = { currentGame | cardA = Nothing, cardB = Nothing }, lastGame = Just { cardA = model.currentGame.cardA, cardB = model.currentGame.cardB, cardC = Just cardC } }

                    else if model.moneyBet > model.money - model.moneyBet then
                        { model | money = model.money - model.moneyBet, moneyBet = model.money - model.moneyBet, currentGame = { currentGame | cardA = Nothing, cardB = Nothing }, lastGame = Just { cardA = model.currentGame.cardA, cardB = model.currentGame.cardB, cardC = Just cardC } }

                    else
                        { model | money = model.money - model.moneyBet, currentGame = { currentGame | cardA = Nothing, cardB = Nothing }, lastGame = Just { cardA = model.currentGame.cardA, cardB = model.currentGame.cardB, cardC = Just cardC } }

                Nothing ->
                    model

        Nothing ->
            model


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Model -> Html Msg
view model =
    div centerHeadlineStyle
        [ h1 [] [ text "ACEY DUCEY CARD GAME" ]
        , p [] [ text "Creative Computing Morristown, New Jersey" ]
        , text """
        Acey-Ducey is played in the following manner. The Dealer (Computer) deals two cards face up. 
        You have an option to bet or not bet depending on whether or not you fell the card will have a value between the first two.
        If you do not want to bet, bet 0.
        """
        , showGame model
        ]


showGame : Model -> Html Msg
showGame model =
    if model.money <= 0 then
        div []
            [ p [] [ text "You lose all you money" ]
            , button [ Html.Events.onClick NewGame ] [ text "Again" ]
            ]

    else
        div []
            [ p [] [ text ("Currently you have " ++ String.fromInt model.money ++ " in your pocket.") ]
            , p [] [ text ("Card A: " ++ cardToString model.currentGame.cardA) ]
            , p [] [ text ("Card B: " ++ cardToString model.currentGame.cardB) ]
            , p [] [ text ("Your current bet is " ++ String.fromInt model.moneyBet) ]
            , input [ type_ "range", Html.Attributes.max (String.fromInt model.money), Html.Attributes.min "0", onInput UpdateBetValue ] []
            , button [ Html.Events.onClick Play ] [ text "Play" ]
            , showLastGame model.lastGame
            , showError model.error
            ]


centerHeadlineStyle : List (Attribute msg)
centerHeadlineStyle =
    [ style "display" "grid"
    , style "place-items" "center"
    ]


showLastGame : Maybe Game -> Html Msg
showLastGame game =
    case game of
        Nothing ->
            h2 [] [ text "First Game" ]

        Just value ->
            div []
                [ h2 [] [ text "Last game" ]
                , p [] [ text ("Last Card A " ++ cardToString value.cardA) ]
                , p [] [ text ("Last Card B " ++ cardToString value.cardB) ]
                , p [] [ text ("Last Card C " ++ cardToString value.cardC) ]
                , showLastWinLose value
                ]


showLastWinLose : Game -> Html Msg
showLastWinLose game =
    Maybe.map3 getGameStateMessage game.cardA game.cardB game.cardC |> Maybe.withDefault (text "something is wrong")


getGameStateMessage : Int -> Int -> Int -> Html Msg
getGameStateMessage cardA cardB cardC =
    if cardA < cardC && cardB > cardC then
        div [] [ text "You won 💵 🎉" ]

    else
        text "You loose 🐳"


showError : Maybe String -> Html Msg
showError value =
    case value of
        Just string ->
            p [] [ text string ]

        Nothing ->
            div [] []


cardToString : Maybe Int -> String
cardToString card =
    case card of
        Just value ->
            if value < 11 then
                String.fromInt value

            else
                case value of
                    11 ->
                        "Jack"

                    12 ->
                        "Queen"

                    13 ->
                        "King"

                    14 ->
                        "Ace"

                    _ ->
                        "impossible value"

        Nothing ->
            "-"


newCard : Random.Generator Int
newCard =
    Random.int 2 14
