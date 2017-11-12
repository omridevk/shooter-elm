module Validation exposing (validateShipX, validateShipY, isBulletsOutOfBound)
import Bullet exposing (Bullet)
import Ship exposing (Ship, updateVx, updateVy)
import Tuple exposing (first, second)


validateShipX (game, cmd) =
    let {width} = game.dimensions
        {ship} = game
        (x, vx) =
            if game.ship.x < 0 then
                (0, 0)
            else if game.ship.x + (toFloat game.ship.width) > toFloat width then
                (toFloat (width - game.ship.width), 0)
            else (game.ship.x, first game.ship.velocity)
        newShip = {ship | x = x}
    in
        ({ game | ship = updateVx newShip vx }, cmd)

validateShipY (game, cmd) =
    let {height} = game.dimensions
        {ship}   = game
        (y, vy) =
            if game.ship.y < 0 then
                (0, 0)
            else if game.ship.y + toFloat game.ship.height > toFloat height then
                (toFloat (height - game.ship.height), 0)
            else (game.ship.y, second game.ship.velocity)
        newShip = {ship | y = y}
    in
        ({ game | ship = updateVy newShip vy}, cmd)

isBulletOutOfBound : Bullet -> Bool
isBulletOutOfBound bullet =
    if bullet.y < 0 then False else True

isBulletsOutOfBound =
    List.filter isBulletOutOfBound
