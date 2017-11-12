import AnimationFrame
import Html exposing (..)
import Html.Attributes exposing (class, src, style)
import Keyboard exposing (KeyCode)
import Keyboard.Extra exposing (Key(..))
import Task
import Time exposing (Time)
import Tuple exposing (first)
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
    , pressedKeys: List Key
    , tick: Time
    , isDead: Bool
    }



type alias Ship =
    { x: Float
    , y: Float
    , velocity: (Float, Float)
    }

type Keys
    = NoKey
    | LeftKey
    | RightKey
    | UpKey
    | DownKey
    | Space

type Msg
    = KeyDown Keys
    | KeyUp Keys
    | KeyboardMsg Keyboard.Extra.Msg
    | SizeUpdated Window.Size
    | Tick Time

initShip : Ship
initShip =
    Ship 0 0 (0, 0)

init: Game
init =
    { dimensions = Window.Size 200 400
    , ship = initShip
    , pressedKeys = []
    , tick = 0
    , isDead = False
    }


subscriptions : Game -> Sub Msg
subscriptions model =
    let ticks =
            AnimationFrame.diffs Tick
    in
        Sub.batch [ keysDown, keysUp, windowDimensionsChanged, Sub.map KeyboardMsg Keyboard.Extra.subscriptions ]

windowDimensionsChanged : Sub Msg
windowDimensionsChanged =
    Window.resizes SizeUpdated


keysUp : Sub Msg
keysUp =
    Keyboard.ups (keysCodeMap False)

keysDown : Sub Msg
keysDown =
    Keyboard.downs (keysCodeMap True)

keysCodeMap : Bool -> Keyboard.KeyCode -> Msg
keysCodeMap downs code =
    case code of
        32 ->
            if downs then KeyDown Space else KeyUp Space

        37 ->
            if downs then KeyDown LeftKey else KeyUp LeftKey

        38 ->
            if downs then KeyDown UpKey else KeyUp UpKey

        39 ->
            if downs then KeyDown RightKey else KeyUp RightKey
        40 ->
            if downs then KeyDown DownKey else KeyUp DownKey

        default ->
            if downs then KeyDown NoKey else KeyUp NoKey

-- update

update: Msg -> Game -> (Game, Cmd msg)
update msg game =
    let {ship} = game
    in
        case msg of
            KeyboardMsg keyMsg ->
                ( { game
                    | pressedKeys = Keyboard.Extra.update keyMsg game.pressedKeys
                  }
                , Cmd.none)
            KeyDown key ->
                (keyDown game key, Cmd.none)
            KeyUp key ->
                (keyUp game key, Cmd.none)
            SizeUpdated dimensions ->
                (game, Cmd.none)
            Tick time ->
                updateGame time game

keyUp: Game -> Keys -> Game
keyUp game key =
    case key of
        UpKey ->
            updateVy game 0
        DownKey ->
            updateVy game 0
        LeftKey ->
            updateVx game 0
        RightKey ->
            updateVx game 0
        _ ->
            game

keyDown : Game -> Keys -> Game
keyDown game key =
     case key of
        UpKey ->
            updateVy game -0.3
        DownKey ->
            updateVy game 0.3
        LeftKey ->
            updateVx game -0.3
        RightKey ->
            updateVx game 0.3
        _ ->
            game

updateVy game vy =
    let (vx, _) = game.ship.velocity
    in
        updateVelocity game (vx, vy)

updateVx game vx =
    let (_, vy) = game.ship.velocity
    in
        updateVelocity game (vx, vy)

updateVelocity : Game -> (Float, Float) -> Game
updateVelocity game (vx, vy) =
    let {ship} =
            game
    in
        {game | ship = {ship | velocity = (vx, vy)}}

updateGame dt game =
    (game, Cmd.none)
        |> applyPhysics dt
        |> validateX

applyPhysics dt (game, cmd) =
    let {ship} = game
        (vx, vy) = ship.velocity
        newShip =
            {ship | x = ship.x + dt * vx
            , y = ship.y + dt * vy
            }
    in
        ({game | ship = newShip}, cmd)

validateX (game, cmd) =
    let {width} = game.dimensions
        (x, vx) =
            if game.ship.x < 0 then
                (0, 0)
            else if game.ship.x + 50 > toFloat game.dimensions.width then
                (toFloat (game.dimensions.width - 50), 0)
            else (game.ship.x, first game.ship.velocity)
        ship = updateShip game.ship x game.ship.y
    in
        (updateVx {game | ship = ship} vx, Cmd.none)

updateShip: Ship -> Float -> Float -> Ship
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
            style
                [ ("width", gameWidth ++ "px")
                , ("background-color", "rgba(0, 0, 0, 0.07)")
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