import AnimationFrame
import Bullet exposing (Bullet, initBullet)
import Enemy
import Game exposing (Game, init)
import Html exposing (div, img, li, text, ul)
import Html.Attributes exposing (class, src, style)
import Keyboard exposing (KeyCode)
import Keyboard.Extra exposing (Key(..))
import List exposing (member, map)
import Random
import Ship exposing (Ship, canFire, initShip, updateVelocity)
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
    | RandomInt Int
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

update: Msg -> Game -> (Game, Cmd Msg)
update msg game =
    let {ship} = game
    in
        case msg of
            KeyboardMsg keyMsg ->
                ( { game
                    | pressedKeys = Keyboard.Extra.update keyMsg game.pressedKeys
                  }
                , Cmd.none)
            RandomInt number ->
                ({game | randomInt = number}, Cmd.none)
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
    let bullets = isBulletsOutOfBound game.dimensions.width game.dimensions.height game.bullets
        isPaused = List.member CharP game.pressedKeys
        newGame = { game | bullets = bullets, paused = isPaused}
    in
        (newGame, Random.generate RandomInt (Random.int 1 game.dimensions.width))
            |> updateTick dt
            |> spawnEnemy
            |> updateEnemiesState
            |> moveEnemies
            |> onKeys
            |> applyShipPhysics dt
            |> applyBulletsPhysics dt
            |> isEnemiesOutOfBound
            |> validateShipX
            |> validateShipY
            |> checkFired
            |> checkCollisions

updateEnemiesState (game, cmd) =
    ({ game | enemies = List.map (updateEnemyState game) game.enemies}, cmd)

updateEnemyState game enemy =
    let {ship} = game
        (dx) =
            enemy.x - ship.x
    in
        if (fleeing enemy) then enemy
        else if (dx > 10) then { enemy | state = Ship.Attacking Ship.Left }
        else if (dx < -10 ) then { enemy | state = Ship.Attacking Ship.Right }
        else { enemy | state = Ship.Idle }

fleeing ship =
    ship.state == Ship.Fleeing Ship.Right || ship.state == Ship.Fleeing Ship.Left

isEnemiesOutOfBound (game, cmd) =
    let enemies = List.filter (\enemy -> (round enemy.y) < game.dimensions.height) game.enemies
    in
        ({game | enemies = enemies}, cmd)


moveEnemy : Ship -> List Bullet -> Ship -> Ship
moveEnemy player bullets enemy =
    let (vx, vy) =
            enemy.velocity
        (dx) =
            enemy.x - player.x
        velocity =
            case enemy.state of
                Ship.Attacking Ship.Right ->
                    (0.2, vy)
                Ship.Attacking Ship.Left ->
                    (-0.2, vy)
                Ship.Fleeing Ship.Right ->
                    (0.2, vy)
                Ship.Fleeing Ship.Left ->
                    (-0.2, vy)
                _ -> (0, vy)
        sortedBullets =
            List.sortBy (\bullet-> (abs ( bullet.y - enemy.y))) bullets
    in
        moveEnemyFromBullet ({enemy | velocity = velocity}) (List.head sortedBullets)

moveEnemyFromBullet : Ship -> Maybe Bullet -> Ship
moveEnemyFromBullet enemy bullet =
    let (vx, vy) =
            enemy.velocity
    in
        case bullet of
            Just bull ->
                let dx =
                        abs (enemy.x - bull.x)
                    dy =
                        bull.y - enemy.y
                in
                    if ( dx < 30.0 && dy < 100 && dy > 0) then {enemy | state = Ship.Fleeing Ship.Right}
                    else { enemy | state = Ship.Idle }
            Nothing ->
                enemy

moveEnemies (game, cmd) =
    let enemies = List.map (moveEnemy game.ship game.bullets) game.enemies
    in
        ({game | enemies = enemies}, cmd)



spawnEnemy (game, cmd) =
    let
        shouldSpawn = (round game.tick) == game.randomInt && List.length game.enemies < 4
        enemy = if shouldSpawn
                then
                    Just (Enemy.initEnemyShip (toFloat (game.elapsed % (game.dimensions.width - 30))) 0 (0, 0.02))
                else Nothing
    in
        case enemy of
            Just enemy ->
                ({game | enemies = enemy :: game.enemies}, cmd)
            Nothing ->
                (game, cmd)

updateTick dt (game, cmd) =
    ({game | tick = dt, elapsed = game.elapsed + round dt}, cmd)


isKeyClicked key pressedKeys =
    member key pressedKeys

isSpaceClicked =
    isKeyClicked Space

checkCollisions (game, cmd) =
    let collisionsMap = List.map (\enemy -> checkCollision enemy) game.enemies
        collisions =
            List.concat (List.map (\collisionFn -> List.map collisionFn game.bullets) collisionsMap)
        newGame = removeCollided game (List.head (maybeListToList collisions))
    in
        (newGame, cmd)

maybeToList : Maybe a -> List a
maybeToList m =
  case m of
    Nothing -> []
    Just x -> [x]

maybeListToList m =
    List.concat (List.map maybeToList m)


removeCollided game collision =
    let (enemies, bullets) =
            case collision of
                Just (collidedShip, collidedBullet) ->
                    ( List.filter (\enemy -> collidedShip /= enemy) game.enemies
                    , List.filter (\bullet -> collidedBullet /= bullet) game.bullets)
                Nothing ->
                    (game.enemies, game.bullets)
    in
        { game | enemies = enemies
        , bullets = bullets
        }

checkCollision ship bullet =
    let collided =
            ship.x < bullet.x + bullet.width
                && ship.x + ship.width > bullet.x
                && ship.y < bullet.y + bullet.height
                && ship.height + ship.y > bullet.y
    in
        if collided then Just (ship, bullet) else Nothing

checkFired (game, cmd) =
    let bullets =
            if isSpaceClicked game.pressedKeys && canFire game.elapsed game.ship.lastFired then
                game.bullets ++ [initBullet game.ship]
            else
                game.bullets
        lastFired =
            if List.length bullets /= List.length game.bullets then game.elapsed else game.ship.lastFired
        {ship} = game
    in
        ( { game | bullets = bullets
          , ship = {ship | lastFired = lastFired} },cmd)

onKeys (game, cmd) =
    let arrows =
            Keyboard.Extra.arrows game.pressedKeys
        wasd =
            Keyboard.Extra.wasd game.pressedKeys
        (x, y) =
            ( if arrows.x == 0 then toFloat wasd.x else toFloat arrows.x
            , if arrows.y == 0 then toFloat wasd.y else toFloat arrows.y
            )
    in
       ({game| ship = (updateVelocity game.ship (x * 0.3, y * -0.3)) }, cmd)

applyBulletsPhysics dt (game, cmd) =
    let {ship} = game
        bullets = map (applyBulletPhysics dt) game.bullets
    in
        ({game | bullets = bullets}, cmd)

applyBulletPhysics dt bullet =
    let (vx, vy) =
            bullet.velocity
    in
        { bullet |
          x = bullet.x + dt * vx
        , y = bullet.y + dt * vy}

applyShipPhysics dt (game, cmd) =
    let {ship} = game
        (vx, vy) = ship.velocity
        enemies = List.map (applyEnemyPhysics dt) game.enemies
        newShip =
            {ship | x = ship.x + dt * vx
            , y = ship.y + dt * vy
            }
    in
        ({game | ship = newShip, enemies = enemies}, cmd)

applyEnemyPhysics dt enemy =
    let (vx, vy) = enemy.velocity
    in
        { enemy | x = enemy.x + dt * vx
        , y = enemy.y + dt * vy
        }

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