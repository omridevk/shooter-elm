module Ship exposing (updateVy, updateVx, updateVelocity, Ship, initShip, render, debug)

import Html exposing (div, img, text)
import Html.Attributes exposing (class, src, style)
import Keyboard.Extra
import Tuple exposing (first, second)

type alias Ship =
    { x: Float
    , y: Float
    , width: Int
    , height: Int
    , velocity: (Float, Float)
    }


initShip : Ship
initShip =
    { x = 0
    , y = 0
    , height = 30
    , width = 30
    , velocity = (0, 0)
    }

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
    let (shipX, shipY, width, height) =
            (toString ship.x, toString ship.y, toString ship.width, toString ship.height)
        shipStyle =
            style [("width", width ++ "px")
                  , ("height", height ++ "px")
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