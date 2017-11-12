import AnimationFrame
import Html exposing (..)
import Html.Attributes exposing (class, src, style)
import Keyboard
import Task
import Time exposing (Time)
import Window


main =
    Html.program
        { init = (init, Cmd.none)
        , update = update
        , view = view
        , subscriptions = subscriptions
        }

-- model

type alias Game =
    { dimensions: Window.Size
    , ship: Ship
    , tick: Time
    , isDead: Bool
    }

type alias Ship =
    { x: Int
    , y: Int
    }

type Keys
    = NoKey
    | LeftKey
    | RightKey
    | UpKey
    | DownKey
    | Space

type Msg
    = KeyPressed Keys
    | SizeUpdated Window.Size
    | Tick Time

initShip : Ship
initShip =
    Ship 0 0

init: Game
init =
    { dimensions = Window.Size 200 400
    , ship = initShip
    , tick = 0
    , isDead = False
    }


subscriptions : Game -> Sub Msg
subscriptions model =
    let ticks =
            AnimationFrame.diffs Tick
    in
        Sub.batch [ keysChanged, windowDimensionsChanged, ticks ]

windowDimensionsChanged : Sub Msg
windowDimensionsChanged =
    Window.resizes SizeUpdated


keysChanged : Sub Msg
keysChanged =
    Keyboard.downs toArrowChanged

toArrowChanged : Keyboard.KeyCode -> Msg
toArrowChanged code =
    case code of
        32 ->
            KeyPressed Space

        37 ->
            KeyPressed LeftKey

        38 ->
            KeyPressed UpKey

        39 ->
            KeyPressed RightKey

        40 ->
            KeyPressed DownKey

        default ->
            KeyPressed NoKey


-- update

update: Msg -> Game -> (Game, Cmd msg)
update msg game =
    case msg of
        KeyPressed key ->
            (moveShip game key, Cmd.none)
        SizeUpdated dimensions ->
            (game, Cmd.none)
        Tick time ->
            updateGame game


updateGame game =
    (game, Cmd.none)
        |> validateX

validateX (game, cmd) =
    let x =
            if game.ship.x < 0 then
                0
            else if game.ship.x > game.dimensions.width then
                game.dimensions.width
            else game.ship.x
    in
        ({game | ship = updateShip game.ship x game.ship.y}, Cmd.none)


moveShip: Game -> Keys -> Game
moveShip game key =
    let (x, y) =
            case key of
                LeftKey ->
                    (game.ship.x - 1, game.ship.y)
                RightKey ->
                    (game.ship.x + 1, game.ship.y)
                UpKey ->
                    (game.ship.x, game.ship.y - 1)
                DownKey ->
                    (game.ship.x, game.ship.y + 1)
                _ ->
                    (game.ship.x, game.ship.y)
    in
        ({game | ship = updateShip game.ship x y})

updateShip: Ship -> Int -> Int -> Ship
updateShip ship x y =
    {ship | x = x
    , y = y
    }


renderShip ship =
    let (shipX, shipY) =
            (toString ship.x, toString ship.y)
        shipStyle =
            style [("width", "50px")
                  , ("height", "50px")
                  , ("transform", "translateX(" ++ shipX ++ "px) translateY(" ++ shipY ++ "px)")
                  ]
        imgStyle =
            style [("max-width", "100%")]
    in
        div [ class "ship", shipStyle] [
            img [ src "./images/fighter.png", imgStyle] []
        ]

renderGame game =

    let (gameWidth, gameHeight) =
            (toString game.dimensions.width, toString game.dimensions.height)
        gameStyle =
            style [ ("width", gameWidth ++ "px")
                  , ("height", gameHeight ++ "px")
                  , ("margin", "auto")]
    in
        div [ class "game", gameStyle ] [
            renderShip game.ship
        ]

view game =
    div [] [
    text (toString game),
    renderGame game
    ]