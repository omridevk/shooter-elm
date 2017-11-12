module Ship exposing (updateVy, updateVx, updateVelocity, Ship, initShip, render, debug)

import Html exposing (div, img, text)
import Html.Attributes exposing (class, src, style)
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


render ship =
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

debug ship =
    div [] [ text (toString ship)]