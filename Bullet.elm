module Bullet exposing (Bullet, initBullet)

import Keyboard.Extra exposing (Key(Space))
import List exposing (member)
import Ship exposing (Ship)

type alias Bullet =
    { x: Float
    , y: Float
    , velocity: Float
    }


initBullet : Ship -> Bullet
initBullet ship =
    Bullet (ship.x + 50 / 2) ship.y -0.3





