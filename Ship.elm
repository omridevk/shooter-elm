module Ship exposing (updateVy, updateVx, updateVelocity, Ship, initShip)

import Keyboard.Extra
import Tuple exposing (first, second)

type alias Ship =
    { x: Float
    , y: Float
    , velocity: (Float, Float)
    }


initShip : Ship
initShip =
    Ship 0 0 (0, 0)

updateVelocity : Ship -> (Float, Float) -> Ship
updateVelocity ship (vx, vy) =
    {ship | velocity = (vx, vy)}


updateVy : Ship -> Float -> Ship
updateVy ship vy =
    let (vx, _) = ship.velocity
    in
        updateVelocity ship (vx, vy)

updateVx : Ship -> Float -> Ship
updateVx ship vx =
    let (_, vy) = ship.velocity
    in
        updateVelocity ship (vx, vy)