module Game exposing (Game, init, render)

import Bullet exposing (Bullet)
import Html exposing (div)
import Html.Attributes exposing (class, style)
import Keyboard.Extra exposing (Key)
import Ship exposing (Ship, initShip, render)
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

render game =
    let (gameWidth, gameHeight) =
            (toString game.dimensions.width, toString game.dimensions.height)
        gameStyle =
            style
                [ ("width", gameWidth ++ "px")
                , ("background-color", "rgba(0, 0, 0, 0.07)")
                , ("position", "relative")
                , ("height", gameHeight ++ "px")
                , ("margin", "auto")]
        ship = Ship.render game.ship
        bullets = List.map Bullet.render game.bullets
    in
        div [ class "game", gameStyle ]
            (ship :: bullets)

