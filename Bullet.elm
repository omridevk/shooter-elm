module Bullet exposing (Bullet, initBullet, render, debug)

import Helpers
import Html exposing (div, img, li, text, ul)
import Html.Attributes exposing (class, style)
import Keyboard.Extra exposing (Key(Space))
import List exposing (member)
import Ship exposing (Ship)

type alias Bullet =
    { x: Float
    , y: Float
    , width: Float
    , height: Float
    , velocity: (Float, Float)
    }


initBullet : Ship -> Bullet
initBullet ship =
    let (vx, vy) =
            ship.velocity
    in
        Bullet (ship.x + (ship.width) / 2) (ship.y - 10) 2 5 (vx, vy - 0.1)


render bullet =
    let (y, x, width, height) = (toString bullet.y, toString bullet.x, toString bullet.width, toString bullet.height)
        (vx, vy) = bullet.velocity
        angle = toString (Helpers.angle vx vy)
        bulletStyle =
            style
                [ ("width", width ++ "px")
                , ("height", height ++ "px")
                , ("background-color", "red")
                , ("left", "0")
                , ("top", "0")
                , ("bottom", "0")
                , ("right", "0")
                , ("transform", "translateY(" ++ y ++ "px) translateX(" ++ x ++ "px) rotateZ(" ++ angle ++"deg)")
                , ("right", "0")
                , ("position", "absolute")
                ]
    in
        div [ class "bullet", bulletStyle]
            [ img [] []
            ]

debug bullets =
    ul [] (List.map (\bullet -> li [] [text (toString bullet)]) bullets )

