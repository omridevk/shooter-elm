module Bullet exposing (Bullet, initBullet, render, debug)

import Html exposing (div, img, li, text, ul)
import Html.Attributes exposing (class, style)
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
    Bullet (ship.x + (toFloat ship.width) / 2) ship.y -0.3


render bullet =
    let (y, x) = (toString bullet.y, toString bullet.x)
        bulletStyle =
            style
                [ ("width", "2px")
                , ("height", "15px")
                , ("background-color", "red")
                , ("left", "0")
                , ("top", "0")
                , ("bottom", "0")
                , ("right", "0")
                , ("transform", "translateY(" ++ y ++ "px) translateX(" ++ x ++ "px")
                , ("right", "0")
                , ("position", "absolute")
                ]
    in
        div [ class "bullet", bulletStyle]
            [ img [] []
            ]

debug bullets =
    ul [] (List.map (\bullet -> li [] [text (toString bullet)]) bullets )

