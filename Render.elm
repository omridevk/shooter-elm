module Render exposing (render)

import Html exposing (div, img, li, text, ul)
import Html.Attributes exposing (class, src, style)

render game =
    div []
        [ renderDebug game
        , renderGame game
        ]


renderShip ship =
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

renderGame game =

    let (gameWidth, gameHeight) =
            (toString game.dimensions.width, toString game.dimensions.height)
        gameStyle =
            style
                [ ("width", gameWidth ++ "px")
                , ("background-color", "rgba(0, 0, 0, 0.07)")
                , ("position", "relative")
                , ("height", gameHeight ++ "px")
                , ("margin", "auto")]
        ship = renderShip game.ship
        bullets = List.map renderBullet game.bullets
    in
        div [ class "game", gameStyle ]
            (ship :: bullets)

renderBullet bullet =
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

containerStyle =
    style [ ("max-width", "400px")
          , ("max-height", "400px")
          , ("overflow-x", "auto")
          , ("position", "absolute")
          ]

renderDebug game =
    div [containerStyle]
        [ renderFps game
        , renderBulletsDebug game.bullets
        , renderShipDebug game.ship
        ]

renderFps game =
    div [] [text ("fps: " ++ (toString (1 / toFloat (round game.tick) * 1000)))]

renderBulletsDebug bullets =
    ul [] (List.map (\bullet -> li [] [text (toString bullet)]) bullets )

renderShipDebug ship =
    div [] [ text (toString ship)]