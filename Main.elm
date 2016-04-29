module Main (..) where

import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Text
import Mouse
import Color exposing (rgb)
import Window


type alias World =
  List Line


type alias Line =
  { position : Position
  , vector : Vector
  }


type alias Position =
  { x : Float
  , y : Float
  }


type alias Vector =
  { length : Float
  , angle : Float
  }


view : ( Int, Int ) -> ( Int, Int ) -> Element
view ( w', h' ) ( x', y' ) =
  let
    ( w, h ) =
      ( toFloat w', toFloat h' )

    rayPosition =
      { x = toFloat x' - (w / 2)
      , y = (h / 2) - toFloat y'
      }
  in
    collage
      w'
      h'
      [ text
          (Text.link
            "http://ncase.me/sight-and-light/"
            (Text.fromString "Based on this excellent tutorial.")
          )
          |> move ( 0, (toFloat h' / 2) - 20 )
      , circle 5 |> filled (rgb 220 0 0) |> move (toXY rayPosition)
      , group (List.map (drawLine defaultLine) world)
      , group
          (List.map (drawLine rayLineStyle) (solveRays rayPosition))
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
start line =
  line.position


end : Line -> Position
end line =
  let
    ( dx, dy ) =
      fromPolar ( line.vector.length, line.vector.angle )
  in
    { x = (line.position.x + dx)
    , y = (line.position.y + dy)
    }


toXY : Position -> ( Float, Float )
toXY p =
  ( p.x, p.y )


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
      ]


solveRays : Position -> List Line
solveRays rayStart =
  world
    |> List.concatMap (toRays rayStart)
    |> List.filterMap curtail


curtail : Line -> Maybe Line
curtail line =
  world
    |> List.filterMap (intersect line)
    |> List.sortBy (.vector >> .length)
    |> List.head


intersect : Line -> Line -> Maybe Line
intersect r s =
  let
    ( r_px, r_py ) =
      toXY (start r)

    ( s_px, s_py ) =
      toXY (start s)

    norms line =
      ( cos line.vector.angle
      , sin line.vector.angle
      )

    ( r_dx, r_dy ) =
      norms r

    ( s_dx, s_dy ) =
      norms s

    sm =
      ((r_px * r_dy) - (r_py * r_dx) + (s_py * r_dx) - (s_px * r_dy))
        / ((s_dx * r_dy) - (s_dy * r_dx))

    rm =
      ((s_px - r_px + (s_dx * sm)) / r_dx)
  in
    if isNaN sm || isNaN rm then
      Nothing
    else if sm < 0 then
      Nothing
    else if s.vector.length < sm then
      Nothing
    else if rm < 0 then
      Nothing
    else
      Just (withLength rm r)


withLength : Float -> Line -> Line
withLength length line =
  let
    vector =
      line.vector
  in
    { line | vector = { vector | length = length } }


adjustAngle : Float -> Line -> Line
adjustAngle delta line =
  let
    vector =
      line.vector
  in
    { line | vector = { vector | angle = vector.angle + delta } }


toRays : Position -> Line -> List Line
toRays position line =
  let
    rayToStart =
      lineBetween position (start line)

    rayToEnd =
      lineBetween position (end line)
  in
    [ adjustAngle (degrees 0.5) rayToStart
    , adjustAngle (degrees -0.5) rayToStart
    , adjustAngle (degrees 0.5) rayToEnd
    , adjustAngle (degrees -0.5) rayToEnd
    ]


lineBetween : Position -> Position -> Line
lineBetween from to =
  { position = from
  , vector = vectorBetween from to
  }


vectorBetween : Position -> Position -> Vector
vectorBetween p1 p2 =
  let
    dx =
      p2.x - p1.x

    dy =
      p2.y - p1.y
  in
    { length = sqrt (dx * dx + dy * dy)
    , angle = atan2 (p2.y - p1.y) (p2.x - p1.x)
    }


world : World
world =
  [ { position = { x = -300, y = -300 }, vector = { length = 600, angle = degrees 0 } }
  , { position = { x = 300, y = -300 }, vector = { length = 600, angle = degrees 90 } }
  , { position = { x = -300, y = -300 }, vector = { length = 600, angle = degrees 90 } }
  , { position = { x = 300, y = 300 }, vector = { length = 600, angle = degrees 180 } }
  , { position = { x = 100, y = 100 }, vector = { length = 50, angle = degrees 315 } }
  , { position = { x = -120, y = 100 }, vector = { length = 50, angle = degrees 250 } }
  , { position = { x = -200, y = 180 }, vector = { length = 150, angle = degrees 250 } }
  , { position = { x = 150, y = -100 }, vector = { length = 120, angle = degrees 235 } }
  , { position = { x = -100, y = -200 }, vector = { length = 400, angle = degrees 90 } }
  , { position = { x = 0, y = -150 }, vector = { length = 300, angle = degrees 90 } }
  ]


main : Signal Element
main =
  Signal.map2
    view
    Window.dimensions
    Mouse.position
