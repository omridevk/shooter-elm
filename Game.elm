module Game exposing (Game, init)

import Bullet exposing (Bullet)
import Keyboard.Extra exposing (Key)
import Ship exposing (Ship, initShip)
import Time exposing (Time)
import Window

type alias Game =
    { dimensions: Window.Size
    , ship: Ship
    , bullets: List Bullet
    , pressedKeys: List Key
    , tick: Time
    , isDead: Bool
    }


init: Game
init =
    { dimensions = Window.Size 200 400
    , ship = initShip
    , bullets = []
    , pressedKeys = []
    , tick = 0
    , isDead = False
    }