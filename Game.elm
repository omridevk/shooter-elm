module Game exposing (Game, init, render)

import Bullet exposing (Bullet)
import Html exposing (div)
import Enemy
import Html.Attributes exposing (class, style)
import Keyboard.Extra exposing (Key)
import Ship exposing (Ship, initShip, render)
import Time exposing (Time)
import Window

type alias Game =
    { dimensions: Window.Size
    , ship: Ship
    , elapsed: Int
    , paused: Bool
    , randomInt: Int
    , bullets: List Bullet
    , enemies: List Ship
    , pressedKeys: List Key
    , tick: Time
    , isDead: Bool
    }


dimensions : Window.Size
dimensions =
    { width = 200
    , height = 400
    }

init: Game
init =
    { dimensions = dimensions
    , ship = initShip
    , enemies = []
    , paused = False
    , bullets = []
    , randomInt = 0
    , pressedKeys = []
    , tick = 0
    , elapsed = 0
    , isDead = False
    }

render game =
    let (gameWidth, gameHeight, elapsed) =
            (toString game.dimensions.width, toString game.dimensions.height, toFloat game.elapsed)
        backgroundPosition = toString (elapsed / 10)
        gameStyle =
            style
                [ ("width", gameWidth ++ "px")
                , ("overflow", "hidden")
                , ("background", "url(images/background.png) repeat")
                , ("backgroundColor", "rgba(0, 0, 0, 0.5)")
                , ("backgroundPositionY", backgroundPosition ++ "px")
                , ("position", "relative")
                , ("height", gameHeight ++ "px")
                , ("margin", "auto")]
        ship = Ship.render game.ship
        bullets = List.map Bullet.render game.bullets
        enemies = List.map Ship.render game.enemies

    in
        div [ class "game", gameStyle ]
            (List.append (ship :: bullets) enemies)

