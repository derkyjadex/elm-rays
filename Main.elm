module Main (..) where

import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Mouse
import Color exposing (rgb)
import Window


type alias World =
  List Line


type Line
  = Line ( Position, Vector )


type X
  = X Float


type Y
  = Y Float


type Position
  = Position ( X, Y )


type Vector
  = Vector ( Length, Angle )


type Length
  = Length Float


type Angle
  = Angle Float


view : ( Int, Int ) -> ( Int, Int ) -> Element
view ( w', h' ) ( x', y' ) =
  let
    ( w, h ) =
      ( toFloat w', toFloat h' )

    ( rayX, rayY ) =
      ( toFloat x' - (w / 2), (h / 2) - toFloat y' )
  in
    collage
      w'
      h'
      [ circle 5 |> filled (rgb 220 0 0) |> move ( rayX, rayY )
      , group (List.map (drawLine defaultLine) world)
      , group (List.map (drawLine rayLineStyle) (solveRays (Position ( X rayX, Y rayY ))))
      ]


wallLineStyle : LineStyle
wallLineStyle =
  { defaultLine
    | width = 4
  }


rayLineStyle : LineStyle
rayLineStyle =
  { defaultLine
    | width = 2
    , color = rgb 220 0 0
  }


start : Line -> Position
start (Line ( position, _ )) =
  position


end : Line -> Position
end (Line ( Position ( X x, Y y ), Vector ( Length length, Angle angle ) )) =
  let
    ( dx, dy ) =
      fromPolar ( length, angle )
  in
    Position ( X (x + dx), Y (y + dy) )


toXY : Position -> ( Float, Float )
toXY (Position ( X x, Y y )) =
  ( x, y )


drawLine : LineStyle -> Line -> Form
drawLine lineStyle line =
  let
    lineStart =
      toXY (start line)

    lineEnd =
      toXY (end line)
  in
    group
      [ segment lineStart lineEnd
          |> traced lineStyle
      , circle 5
          |> filled (rgb 220 0 0)
          |> move lineStart
      , circle 5
          |> filled (rgb 220 0 0)
          |> move lineEnd
      ]


solveRays : Position -> List Line
solveRays rayStart =
  List.concatMap (toRays rayStart) world


toRays : Position -> Line -> List Line
toRays position line =
  [ start line
  , end line
  ]
    |> List.map (toLine position)


toLine : Position -> Position -> Line
toLine from to =
  Line
    ( from
    , vectorToPoint from to
    )


vectorToPoint : Position -> Position -> Vector
vectorToPoint (Position ( X x1, Y y1 )) (Position ( X x2, Y y2 )) =
  let
    dx =
      x2 - x1

    dy =
      y2 - y1
  in
    Vector
      ( Length (sqrt (dx * dx + dy * dy))
      , Angle (atan2 (y2 - y1) (x2 - x1))
      )


world : World
world =
  [ Line ( Position ( X -300, Y -300 ), Vector ( Length 600, Angle <| degrees 0 ) )
  , Line ( Position ( X 300, Y -300 ), Vector ( Length 600, Angle <| degrees 90 ) )
  , Line ( Position ( X -300, Y -300 ), Vector ( Length 600, Angle <| degrees 90 ) )
  , Line ( Position ( X 300, Y 300 ), Vector ( Length 600, Angle <| degrees 180 ) )
  , Line ( Position ( X 100, Y 100 ), Vector ( Length 50, Angle <| degrees -45 ) )
  , Line ( Position ( X -120, Y 100 ), Vector ( Length 50, Angle <| degrees -110 ) )
  , Line ( Position ( X -200, Y 180 ), Vector ( Length 150, Angle <| degrees -110 ) )
  , Line ( Position ( X 150, Y -100 ), Vector ( Length 120, Angle <| degrees -125 ) )
  ]


main : Signal Element
main =
  Signal.map2
    view
    Window.dimensions
    Mouse.position
