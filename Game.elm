import AnimationFrame
import Html exposing (div, img, li, text, ul)
import Html.Attributes exposing (class, src, style)
import Keyboard exposing (KeyCode)
import Keyboard.Extra exposing (Key(..))
import List exposing (member, map)
import Task
import Time exposing (Time)
import Tuple exposing (first, second)
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
    , bullets: List Bullet
    , pressedKeys: List Key
    , tick: Time
    , isDead: Bool
    }

type alias Bullet =
    { x: Float
    , y: Float
    , velocity: Float
    }

type alias Ship =
    { x: Float
    , y: Float
    , velocity: (Float, Float)
    }


type Msg
    = KeyboardMsg Keyboard.Extra.Msg
    | SizeUpdated Window.Size
    | Tick Time

initShip : Ship
initShip =
    Ship 0 0 (0, 0)

init: Game
init =
    { dimensions = Window.Size 200 400
    , ship = initShip
    , bullets = []
    , pressedKeys = []
    , tick = 0
    , isDead = False
    }


subscriptions : Game -> Sub Msg
subscriptions model =
    let ticks =
            AnimationFrame.diffs Tick
    in
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
            Tick time ->
                updateGame time game

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
        |> updateShip
        |> applyPhysics dt
        |> validateX
        |> validateY
        |> checkFired
        |> checkBullets

initBullet : Ship -> Bullet
initBullet ship =
    Bullet ship.x ship.y -0.3


checkBullets (game, cmd) =
    ({game | bullets = List.filter (validateBullet game) game.bullets}, cmd)

validateBullet : Game -> Bullet -> Bool
validateBullet game bullet =
    let {dimensions} = game
    in
        if bullet.y < 0 then False else True

checkFired (game, cmd) =
    let bullets =
            if member Space game.pressedKeys then game.bullets ++ [initBullet game.ship] else game.bullets
    in
        ({game | bullets = bullets}, cmd)

updateShip (game, cmd) =
    let arrows =
            Keyboard.Extra.arrows game.pressedKeys
        (x, y) =
            (toFloat arrows.x, toFloat arrows.y)
    in
        (updateVelocity game (x * 0.3, y * -0.3), cmd)


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

validateX (game, cmd) =
    let {width} = game.dimensions
        {ship} = game
        (x, vx) =
            if game.ship.x < 0 then
                (0, 0)
            else if game.ship.x + 50 > toFloat width then
                (toFloat (width - 50), 0)
            else (game.ship.x, first game.ship.velocity)
        newShip = {ship | x = x}
    in
        (updateVx {game | ship = newShip} vx, Cmd.none)

validateY (game, cmd) =
    let {height} = game.dimensions
        {ship}   = game
        (y, vy) =
            if game.ship.y < 0 then
                (0, 0)
            else if game.ship.y + 50 > toFloat height then
                (toFloat (height - 50), 0)
            else (game.ship.y, second game.ship.velocity)
        newShip = {ship | y = y}
    in
        (updateVy {game | ship = newShip} vy, cmd)

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
                , ("position", "relative")
                , ("height", gameHeight ++ "px")
                , ("margin", "auto")]
        ship = [renderShip game.ship]
        bullets = map renderBullet game.bullets
    in
        div [ class "game", gameStyle ]
            (ship ++ bullets)

renderBullet bullet =
    let (y, x) = (toString bullet.y, toString bullet.x)
        bulletStyle =
            style
                [ ("width", "10px")
                , ("height", "10px")
                , ("background-color", "red")
                , ("left", "0")
                , ("top", "0")
                , ("bottom", "0")
                , ("right", "0")
                , ("transform", "translateY(" ++ y ++ "px) translateX(" ++ x ++ "px")
                , ("right", "0")
                , ("position", "absolute")
                ]
    in
        div [ class "bullet", bulletStyle]
            [ img [] []
            ]

containerStyle =
    style [ ("max-width", "400px")
          , ("max-height", "400px")
          , ("overflow-x", "auto")
          , ("position", "absolute")
          ]

renderDebug game =
    div [containerStyle]
        [ renderBulletsDebug game.bullets
        , renderShipDebug game.ship
        ]


renderBulletsDebug bullets =
    ul [] (List.map (\bullet -> li [] [text (toString bullet)]) bullets )

renderShipDebug ship =
    div [] [ text (toString ship)]

view game =
    div []
        [ renderDebug game
        , renderGame game
        ]