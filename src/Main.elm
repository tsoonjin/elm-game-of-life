module Main exposing (..)

import Task
import Time
import Browser
import Random
import Randomizer exposing (generateInt)
import Array exposing (Array)
import String
import Grid exposing (initGrid, initRandomGrid, initWorld, Grid, getCell, flatten, tickWorld, initEmptyWorld, setCellValue, initWorldWithSeed)
import Html exposing (Html, button, div, text, span)
import Html.Events exposing (onClick)
import Html.Attributes exposing (style, disabled)


initialSeed = Random.initialSeed 314
colors = Array.fromList ["#090910", "#A9333A", "#E1578A", "#864879", "#f28482"]
bgColors = Array.fromList ["#ffffff", "#000000"]

-- MAIN


main =
  Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }



-- MODEL


type alias Model = {
    gameStateText: String,
    canStartGame: Bool,
    isGameAlive: Bool,
    height: Int,
    width: Int,
    generation: Int,
    cellHeight: Int,
    cellWidth: Int,
    bgColor: String,
    colorChoices: Array String,
    seed: Random.Seed,
    world: Grid
    }


getTime : Cmd Msg
getTime =
    Task.perform GetTime Time.now

init : () -> (Model, Cmd Msg)

init _ = ({
    gameStateText = "Play",
    canStartGame = False,
    isGameAlive =  True,
    height = 300,
    width = 300,
    generation = 0,
    colorChoices = colors,
    bgColor = Maybe.withDefault "#12e" (Array.get 0 colors),
    cellWidth = 30,
    cellHeight = 30,
    seed = initialSeed,
    world = Array.repeat 10 (Array.repeat 10 0)
    }, getTime)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every 1000 Tick


-- UPDATE


type Msg = Tick Time.Posix | NewWorld Grid | GetTime Time.Posix | ControlGame | ToggleCell Int Int | InitRandom | Reset


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Reset ->
        let
            newWorld = initEmptyWorld 10 10
            generation = 0
            isGameAlive = True
            canStartGame = False
        in
            ({ model | world = newWorld, generation = generation, isGameAlive = isGameAlive, canStartGame = canStartGame, gameStateText = "Play" }, Cmd.none)
    InitRandom ->
        let
            (newWorld, newSeed) = initWorldWithSeed model.seed 10 10 1
        in
        ({ model | world = newWorld, seed = newSeed }, Cmd.none)
    ToggleCell col row ->
        let
            currCol = Maybe.withDefault (Array.fromList []) (Array.get row model.world)
            currCell = Maybe.withDefault 0 (Array.get col currCol)
            newCell = modBy 2 (currCell + 1)
            newWorld = setCellValue newCell col row model.world
        in
            if model.canStartGame then
                (model, Cmd.none)
            else
                ({ model | world = newWorld }, Cmd.none)
    ControlGame ->
        let
            canStartGame = not model.canStartGame
            gameStateText = if canStartGame then "Pause" else "Play"
        in
            ({ model | canStartGame = canStartGame, gameStateText = gameStateText }, Cmd.none)
    NewWorld newWorld -> ({ model | world = newWorld }, Cmd.none)
    GetTime currentTime ->
        let
            seedNow = Random.initialSeed (Time.posixToMillis currentTime)
        in
        -- ({ model | seed = seedNow },
        --     Task.succeed (initWorld seedNow  10 10 1) |> Task.perform NewWorld
        -- )
        ({ model | seed = seedNow, world = initEmptyWorld 10 10}, Cmd.none)
    Tick _ ->
        let
            (idx, nextSeed) = generateInt (0, (Array.length model.colorChoices) - 1) model.seed
            nextColor = Array.get idx model.colorChoices
            nextWorld = tickWorld model.world
            isGameAlive = nextWorld /= model.world
            nextGeneration =
                case isGameAlive of
                    False -> model.generation
                    True -> model.generation + 1
        in
            if model.canStartGame then
                ({ model | bgColor = Maybe.withDefault model.bgColor nextColor, seed = nextSeed, world = nextWorld, generation = nextGeneration, isGameAlive = isGameAlive}, Cmd.none)
            else
                (model, Cmd.none)


-- VIEW

generateBox: Int -> Int -> Int -> Int -> String -> Html Msg
generateBox col row width height color =
      button [style "width" ((String.fromInt width) ++ "px"), style "height" ((String.fromInt height)  ++ "px"), style "background-color" color, onClick (ToggleCell col row)] []

world2HTML: Int -> Int -> List Int -> List (Html Msg)
world2HTML width height world =
    List.indexedMap (\idx -> \state -> generateBox (remainderBy 10 idx) (idx // 10) width height (Maybe.withDefault "#ffffff" (Array.get state bgColors))) world

view : Model -> Html Msg
view model =
  div [style "display" "flex", style "flex-direction" "column", style "gap" "72px"]
  [
    span [style "margin" "auto", style "display" "block"] [text "Game of Life: ELM"],
    div [style "margin" "auto"]
    [
        button [ style "width" "100px", style "margin-right" "24px", onClick ControlGame, disabled (not model.isGameAlive)] [ text model.gameStateText ],
        button [ style "width" "100px", style "margin-right" "24px", onClick InitRandom, disabled (not model.isGameAlive)] [ text "Random" ],
        button [ style "width" "100px", style "margin-right" "24px", onClick Reset] [ text "Reset" ]
    ],
  (div [style "background-color" "#fffff6", style "width" ((String.fromInt model.width) ++ "px"), style "height" ((String.fromInt model.height) ++ "px"), style "display" "flex", style "flex-wrap" "wrap", style "align-content" "flex-start", style "margin" "auto"]
  (world2HTML model.cellWidth model.cellHeight (flatten model.world))),
    span [style "margin" "auto", style "display" "block"] [text (if model.isGameAlive then ("Generation: " ++ String.fromInt model.generation) else ("RIP. You have survived " ++ (String.fromInt model.generation) ++ " generation(s)"))
  ]
  ]
