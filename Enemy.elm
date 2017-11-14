module Enemy exposing (initEnemyShip, render)
import Html exposing (div)
import Ship exposing (Ship)



initEnemyShip : Float -> Float -> (Float, Float) -> Ship
initEnemyShip x y (vx, vy) =
    { x = x
    , y = y
    , state = Ship.Idle
    , lastFired = 0
    , height = 40
    , width = 40
    , velocity = (vx, vy)
    }


render enemies =
    div [] (List.map (\enemy -> Ship.render enemy) enemies)