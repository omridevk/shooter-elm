module Validation exposing (validateShipX, validateShipY, isBulletsOutOfBound)
import Bullet exposing (Bullet)
import Ship exposing (Ship, updateVx, updateVy)
import Tuple exposing (first, second)


validateShipX (game, cmd) =
    let {width} = game.dimensions
        gameWidth = toFloat width
        {ship} = game
        (x, vx) =
            if game.ship.x < 0 then
                (0, 0)
            else if game.ship.x + (game.ship.width) > gameWidth then
                (gameWidth - game.ship.width, 0)
            else (game.ship.x, first game.ship.velocity)
        newShip = {ship | x = x}
    in
        ({ game | ship = updateVx newShip vx }, cmd)

validateShipY (game, cmd) =
    let {height} = game.dimensions
        gameHeight = toFloat height
        {ship}   = game
        (y, vy) =
            if game.ship.y < 0 then
                (0, 0)
            else if game.ship.y + game.ship.height > gameHeight then
                (gameHeight - game.ship.height, 0)
            else (game.ship.y, second game.ship.velocity)
        newShip = {ship | y = y}
    in
        ({ game | ship = updateVy newShip vy}, cmd)

isBulletOutOfBound : Int -> Int -> Bullet -> Bool
isBulletOutOfBound width height bullet =
    not (bullet.y < 0 || bullet.y > toFloat height || bullet.x < 0 || bullet.x > toFloat width)

isBulletsOutOfBound width height =
    List.filter (isBulletOutOfBound width height)
