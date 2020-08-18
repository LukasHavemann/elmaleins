module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes as Attributes exposing (attribute, class, id, name, placeholder, size, type_, value)
import Html.Events exposing (..)
import List exposing (range)
import Random
import Time



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL
type alias Guess =
    { guessedNumber : Maybe Int
    , correspondingChallenge : Challenge
    }

type ChallengeType = Result | FactorA | FactorB

type alias Challenge =
    { factorA : Int
    , factorB : Int
    , result : Int
    , challengeType : ChallengeType
    }


type alias FactorPool =
    { firstElement : Int
    , furtherElements : List Int
    , changes : String
    }

-- TODO niels 08.08.2020: Config must become persistent.
type alias Config =
    { poolA : FactorPool
    , poolB : FactorPool
    , timeoutInSeconds : Int
    , -- for reverse Challenges we only show the result and the user must guess the factors.
      reverseChallenges : Bool
    , show : Bool
    }


type alias Model =
    { config : Config
    , currentChallenge : Maybe Challenge
    , currentGuessedValue : Maybe Int
    , remainingTime : Int
    , guesses : List Guess
    }


createChallenge : Int -> Int -> Challenge
createChallenge a b = Challenge a b (a * b) Result

createGuess : Maybe Int -> Challenge -> Guess
createGuess v c = Guess v c

getToBeGuessed : Challenge -> Int
getToBeGuessed challenge = 
    case challenge.challengeType of
        Result -> challenge.result 
        FactorA -> challenge.factorA
        FactorB -> challenge.factorB

randomFactor : FactorPool -> Random.Generator Int
randomFactor factorPool =
    Random.uniform factorPool.firstElement factorPool.furtherElements


challengeGen : FactorPool -> FactorPool -> Random.Generator Challenge
challengeGen factorPoolA factorPoolB =
    Random.map2
        (\a b -> createChallenge a b)
        (randomFactor factorPoolA)
        (randomFactor factorPoolB)


guessCorrect : Guess -> Bool
guessCorrect guess =
    case guess.guessedNumber of 
        Nothing         -> False
        Just number     -> (getToBeGuessed guess.correspondingChallenge) == number

guessNotCorrect : Guess -> Bool
guessNotCorrect guess = not (guessCorrect guess)

init : () -> ( Model, Cmd Msg )
init _ =
    -- TODO niels 08.08.2020: Local-Storage must be added.
    -- https://elmprogramming.com/saving-app-state.html
    -- https://package.elm-lang.org/packages/billstclair/elm-localstorage/latest/
    let
        listA =
            range 3 14

        listB =
            range 10 14
    in
    ( Model (Config (FactorPool 2 listA "") (FactorPool 1 listB "") 20 False False) Maybe.Nothing Maybe.Nothing 0 []
    , Cmd.none
    )



-- UPDATE


type Msg
    = StartChallenges
    | StopChallenges
    | NewChallenge Challenge
    | Tick Challenge Time.Posix
    | Guessed (Maybe Int) Challenge
    | ShowConfig Config
    | HideConfig Config
    | NewGuessedValue String
    | ChangeTimeout String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeTimeout newTime ->
            ( { model | config = setTimeoutInSeconds model.config (String.toInt newTime) }, Cmd.none )

        ShowConfig config ->
            let
                newConfig =
                    { config | show = True }
            in
            ( { model | config = newConfig }, Cmd.none )

        HideConfig config ->
            let
                newConfig ={ config | show = False }
            in
            ( { model | config = newConfig }, Cmd.none )

        StartChallenges ->
            ( { model | guesses = [] }
            , Random.generate NewChallenge (challengeGen model.config.poolA model.config.poolB)
            )

        StopChallenges ->
            ( { model | currentChallenge = Nothing }, Cmd.none )

        NewGuessedValue  value ->
            ( { model | currentGuessedValue =  String.toInt value}, Cmd.none )

        Guessed currentGuessedValue currentChallenge ->
            let 
                guess = createGuess currentGuessedValue currentChallenge
            in
            ( { model | currentChallenge = Nothing, 
                        currentGuessedValue = Nothing,
                        guesses = guess :: model.guesses }
            , if numberOf guessNotCorrect (guess :: model.guesses) < 3 then
                Random.generate NewChallenge (challengeGen model.config.poolA model.config.poolB)
              else
                Cmd.none
            )

        NewChallenge newChallenge ->
            ( { model | currentChallenge = Just newChallenge, remainingTime = model.config.timeoutInSeconds }
            , Cmd.none
            )

        Tick challenge _ ->
            { model | remainingTime = model.remainingTime - 1 }
                |> (if model.remainingTime <= 1 then
                        update (Guessed Nothing challenge)

                    else
                        \m -> ( m, Cmd.none )
                   )


setTimeoutInSeconds : Config -> Maybe Int -> Config
setTimeoutInSeconds config newTime =
    case newTime of
        Nothing     -> config
        Just time   -> { config | timeoutInSeconds = time }


-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
    case model.currentChallenge of
        Nothing     -> Sub.none
        Just c      -> Time.every 1000 (Tick c)



-- VIEW
view : Model -> Html Msg
view model =
    div [ class "container-sm" ]
        [ h1 [] [ text "1x1-Trainer" ]
        , showConfiguration model
        , showControl model
        , showMaybeChallenge model
        , showResults model
        ]


showConfiguration : Model -> Html Msg
showConfiguration model =
    if model.config.show then
        div [ id "configuration" ]
            [ h2 [] [ text "Konfiguration" ]
            , showFactorConfig model.config.poolA "A"
            , showFactorConfig model.config.poolB "B"
            , div [ class "input-group", class "input-group-sm", class "mb-3" ]
                [ input
                    [ type_ "number"
                    , class "form-control"
                    , name "timeout"
                    , Attributes.min "2"
                    , Attributes.max "30"
                    , size 2
                    , attribute "aria-label" "Definiere den Timeout."
                    , onInput ChangeTimeout
                    , value (String.fromInt model.config.timeoutInSeconds)
                    ]
                    []
                , div [ class "input-group-append" ]
                    [ span [ class "input-group-text" ] [ text " s Zeit " ]
                    ]
                , div [ class "input-group-text" ]
                    [ input [ type_ "checkbox", attribute "aria-label" "Umgekehrt" ] []
                    ]
                , div [ class "input-group-append" ]
                    [ span [ class "input-group-text" ] [ text " Umgekehrt" ]
                    ]
                ]
            , hr [] []
            ]

    else
        text ""



-- TODO niels 17.08.2020: Input and Button needs Actions.


showFactorConfig : FactorPool -> String -> Html Msg
showFactorConfig config configName =
    div [ id ("factor" ++ configName ++ "Config") ]
        [ p []
            [ strong [] [ text ("Faktor " ++ configName ++ ": ") ]
            , text (String.join ", " (List.map String.fromInt (config.firstElement :: config.furtherElements)))
            ]
        , div [ class "input-group", class "input-group-sm", class "mb-3" ]
            [ input
                [ type_ "text"
                , class "form-control"
                , size 10
                , placeholder "3-5, 8"
                , Attributes.name ("range_" ++ configName)
                , attribute "aria-label" ("Faktoren für " ++ configName)
                ]
                []
            , div [ class "input-group-append" ]
                [ button [ type_ "button", class "btn", class "btn-success", name ("add_" ++ configName) ] [ text "Hinzufügen" ]
                , button [ type_ "button", class "btn", class "btn-warning", name ("remove_" ++ configName) ] [ text "Entfernen" ]
                ]
            ]
        ]


showControl : Model -> Html Msg
showControl model =
    div [ id "control" ]
        [ if model.config.show then
            button [ type_ "button", class "btn", class "btn-primary", name "hide", onClick (HideConfig model.config) ] [ text "Verdecke Config" ]

          else
            button [ type_ "button", class "btn", class "btn-primary", name "show", onClick (ShowConfig model.config) ] [ text "Zeige Config" ]
        , text " "
        , case model.currentChallenge of
            Nothing ->
                button [ type_ "button", class "btn", class "btn-primary", name "start", onClick StartChallenges ] [ text "Start" ]

            Just _ ->
                button [ type_ "button", class "btn", class "btn-primary", name "start", onClick StopChallenges ] [ text "Stop" ]
        ]


showMaybeChallenge : Model -> Html Msg
showMaybeChallenge model =
    case model.currentChallenge of
        Nothing -> text ""
        Just c  -> showCurrentChallenge c model.remainingTime model.currentGuessedValue



-- TODO niels 17.08.2020: Reverse must be implememted.


showCurrentChallenge : Challenge -> Int -> Maybe Int -> Html Msg
showCurrentChallenge challenge remainingTime currentGuessedValue =
    div [ id "challenge" ]
        [ h2 [] [ text "Aufgabe" ]
        , p [] [ text ("Noch " ++ String.fromInt remainingTime ++ " Sekunden") ]
        , div [ class "input-group", class "input-group-sm", class "mb-3" ]
            [ div [ class "input-group-prepend" ]
                [ span [ class "input-group-text" ] [ text ((String.fromInt challenge.factorA) ++ " x " ++ (String.fromInt challenge.factorB) ++ " = ") ]
                ]
            , input
                [ type_ "number"
                , class "form-control"
                , Attributes.min "2"
                , Attributes.max "900"
                , size 3
                , name "result"
                , attribute "aria-label" "Ergebnis"
                , value (toStr "" currentGuessedValue)
                , onInput (NewGuessedValue)
                ]
                []
            , div [ class "input-group-append" ]
                [ button [ type_ "button", class "btn", class "btn-success", name "next", onClick (Guessed currentGuessedValue challenge) ] [ text "Abgeben" ]
                ]
            ]
        ]


showResults : Model -> Html Msg
showResults model =
    if List.isEmpty model.guesses then
        text ""

    else
        div [ id "results" ]
            ([ hr [] []
             , h2 [] [ text "Ergebnisse" ]
             , showSuccessRate model.guesses
             ]
                ++ showGuesses model.guesses
            )


showSuccessRate : List Guess -> Html Msg
showSuccessRate guesses =
    p [] [ text (String.fromInt (numberOf guessCorrect guesses) ++ " von " ++ String.fromInt (List.length guesses) ++ " Richtig.") ]


showGuesses : List Guess -> List (Html Msg)
showGuesses guess =
    List.map showGuess guess


showGuess : Guess -> Html Msg
showGuess guess =
    if guessCorrect guess then
        p [ class "alert", class "alert-success", attribute "role" "alert" ]
            [ text ("Richtig " ++ challengeToString guess.correspondingChallenge) ]

    else
        p [ class "alert", class "alert-info", attribute "role" "alert" ]
            [ text ("Nicht ganz " ++ challengeToString guess.correspondingChallenge ++ " und nicht " ++ toStr "<keine Eingabe>" guess.guessedNumber ) ]


numberOf: (Guess -> Bool) -> List Guess -> Int
numberOf filterFunc guesses =
    List.length (List.filter filterFunc guesses)

challengeToString : Challenge -> String
challengeToString challenge =
    String.fromInt challenge.factorA ++ " x " ++ String.fromInt challenge.factorB ++ " = " ++  String.fromInt challenge.result

toStr : String -> Maybe Int -> String
toStr default value =
    case value of
        Nothing -> default
        Just v  -> String.fromInt v
