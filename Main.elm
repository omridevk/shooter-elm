import AnimationFrame
import Bullet exposing (Bullet, initBullet)
import Game exposing (Game, init)
import Html exposing (div, img, li, text, ul)
import Html.Attributes exposing (class, src, style)
import Keyboard exposing (KeyCode)
import Keyboard.Extra exposing (Key(..))
import List exposing (member, map)
import Ship exposing (Ship, initShip, updateVelocity)
import Task
import Time exposing (Time)
import Tuple exposing (first, second)
import Validation exposing (isBulletsOutOfBound, validateShipX, validateShipY)
import Window


main =
    Html.program
        { init = (init, Cmd.none)
        , update = update
        , view = render
        , subscriptions = subscriptions
        }

-- model



type Msg
    = KeyboardMsg Keyboard.Extra.Msg
    | SizeUpdated Window.Size
    | KeyMsg Keyboard.KeyCode
    | Tick Time




subscriptions : Game -> Sub Msg
subscriptions game =
    let ticks =
            AnimationFrame.diffs Tick
    in
        if game.paused then
            Sub.batch [Keyboard.downs KeyMsg]
        else
            Sub.batch [ windowDimensionsChanged, Sub.map KeyboardMsg Keyboard.Extra.subscriptions, ticks ]

windowDimensionsChanged : Sub Msg
windowDimensionsChanged =
    Window.resizes SizeUpdated


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
            SizeUpdated dimensions ->
                (game, Cmd.none)
            KeyMsg keyCode ->
                handlePause game keyCode
            Tick time ->
                updateGame time game

handlePause game keyCode =
    case keyCode of
        80 ->
            ({game | paused = False, pressedKeys = []}, Cmd.none)
        _ ->
            (game, Cmd.none)

updateGame dt game =
    let bullets = isBulletsOutOfBound game.bullets
        isPaused = List.member CharP game.pressedKeys
        newGame = { game | bullets = bullets, paused = isPaused}
    in
        (newGame, Cmd.none)
            |> updateTick dt
            |> onKeys
            |> applyPhysics dt
            |> validateShipX
            |> validateShipY
            |> checkFired

updateTick dt (game, cmd) =
    ({game | tick = dt, elapsed = game.elapsed + round dt}, cmd)


checkFired (game, cmd) =
    let bullets =
            if member Space game.pressedKeys then game.bullets ++ [initBullet game.ship] else game.bullets
    in
        ({game | bullets = bullets}, cmd)

onKeys (game, cmd) =
    let arrows =
            Keyboard.Extra.arrows game.pressedKeys
        (x, y) =
            (toFloat arrows.x, toFloat arrows.y)
    in
       ({game| ship = (updateVelocity game.ship (x * 0.3, y * -0.3)) }, cmd)

applyPhysics dt (game, cmd) =
    let {ship} = game
        (vx, vy) = ship.velocity
        newShip =
            {ship | x = ship.x + dt * vx
            , y = ship.y + dt * vy
            }
        bullets = map (\bullet -> {bullet | y = bullet.y + dt * bullet.velocity}) game.bullets
    in
        ({game | ship = newShip, bullets = bullets}, cmd)


-- view

render game =
    div []
        [ renderDebug game
        , Game.render game
        ]


containerStyle =
    style [ ("max-width", "400px")
          , ("max-height", "400px")
          , ("overflow-x", "auto")
          , ("position", "absolute")
          ]

renderDebug game =
    div [containerStyle]
        [ renderFps game
        , Bullet.debug game.bullets
        , Ship.debug game.ship
        , text (toString game)
        ]

renderFps game =
    div [] [text ("fps: " ++ (toString (round (1 / toFloat (round game.tick) * 1000))))]