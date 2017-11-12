import Html exposing (..)
import Html.Events exposing (onClick)
import Html.Attributes exposing (..)
import AnimationFrame
import Keyboard
import Time exposing (Time)


main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

type alias Position =
    { x: Int , y: Int}

-- MODEL

type alias Model =
    { tick : Time
    , shooting: Bool
    , game: Game
    , position: Position
    }

position: Position
position =
    { x = 0
    , y = 0
    }

type alias Game =
    { width: Int
    , height: Int
    }

game =
    { width = 200
    , height = 400}

model: Model
model =
    { tick = 0
    , position = position
    , game = game
    , shooting = False
    }

init : (Model, Cmd Msg)
init =
  (model, Cmd.none)

-- UPDATE

type Msg
  = Tick Time
    | Move Int Int
    | Shooting Bool
    | NoOp

setPosition: Position -> Model -> Model
setPosition newPos model =
    { model | position = newPos }


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick newTime ->
        ({model | tick = newTime}, Cmd.none)
    Move x y ->
        let
            pos =
                model.position.x + x
                |> setX model.position
            position =
                { x = model.position.x + x
                , y = model.position.y + y}
        in
            ((setPosition pos model), Cmd.none)
    Shooting flag->
        ({ model | shooting = flag}, Cmd.none)
    NoOp ->
        (model, Cmd.none)

setX position x =
    setGameX model.game position x

setGameX game position x =
    let
        setX = setPosX position
        posX = if position.x + 50 > game.width then game.width - 50 else x
    in
         setPosX position posX

setPosX position x =
    {position | x = x}

view : Model -> Html Msg
view model =
     div []
        [ h1 [] [ text (toString model)]
        , div [ class "game" , gameStyle "200" "400" ] [
          (renderDefaultShip model)
        ]
        ]

gameStyle : String -> String -> Attribute msg
gameStyle width height =
    style
        [ ("width", width ++ "px")
        , ("position", "relative")
        , ("overflow", "hidden")
        , ("height", height ++ "px")
        , ("margin", "auto")
        ]

shipStyle : Model -> Attribute msg
shipStyle model =
    let xString = (toString model.position.x ) ++ "px"
        yString = (toString model.position.y ) ++ "px"
        transform = "translateX(" ++ xString ++ ")" ++ " translateY(" ++ yString ++ ")"
    in
        style
            [ ("height", "50px")
            , ("width", "50px")
            , ("position", "absolute")
            , ("transform", transform)
        ]

renderShip width height model =
    div [ shipStyle model, class "ship"] [
        img [ src "/images/fighter.png"
        , style [("width", (toString width) ++ "px"), ("height", (toString height) ++ "px")]
        ]
        []
    ]

renderDefaultShip =
    renderShip 50 50

keyCodeToMsg : Bool -> Keyboard.KeyCode -> Msg
keyCodeToMsg downs keyCode =
    case keyCode of
        -- down
        40 -> Move 0 10
        -- up
        38 -> Move 0 -10
        -- left
        37 -> Move -10 0
        -- right
        39 -> Move 10 0

        32 -> Shooting downs

        _ -> NoOp

subscriptions : Model -> Sub Msg
subscriptions model =
  let
    ticks =
        AnimationFrame.diffs Tick
    keysDown =
        Keyboard.downs (keyCodeToMsg True)
    keyUp =
        Keyboard.ups (keyCodeToMsg False)
  in
    Sub.batch [ ticks , keysDown, keyUp]