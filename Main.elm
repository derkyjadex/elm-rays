module Main (..) where

import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Mouse
import Color exposing (rgb)
import Window
import Maybe exposing (withDefault)


type alias World =
  List Line


type alias Line =
  ( Position, Vector )


type alias Position =
  ( Length, Length )


type alias Vector =
  ( Angle, Length )


type alias Length =
  Float


type alias Angle =
  Float


view : ( Int, Int ) -> ( Int, Int ) -> Element
view ( w', h' ) ( x', y' ) =
  let
    ( w, h ) =
      ( toFloat w', toFloat h' )

    ( x, y ) =
      ( toFloat x' - (w / 2), (h / 2) - toFloat y' )
  in
    collage
      w'
      h'
      [ group (List.map (drawLine defaultLine) world)
      , circle 5 |> filled (rgb 220 0 0) |> move ( x, y )
      , group (List.map (drawLine rayLineStyle) (solveRays ( x, y )))
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


drawLine : LineStyle -> Line -> Form
drawLine lineStyle ( ( x, y ), ( angle, t ) ) =
  let
    ( x', y' ) =
      fromPolar ( t, degrees angle )

    end =
      ( x + x', y + y' )
  in
    path [ ( x, y ), end ] |> traced lineStyle


solveRays : Position -> List Line
solveRays start =
  let
    angles =
      List.map ((*) 10) [1..37]
  in
    List.map (\a -> ( start, ( a, solveRay start a ) )) angles


solveRay : Position -> Angle -> Length
solveRay start angle =
  let
    lengths =
      List.filterMap (distanceToLine start angle) world
  in
    List.minimum lengths |> withDefault 0


distanceToLine : Position -> Angle -> Line -> Maybe Length
distanceToLine ( rx, ry ) ra ( ( sx, sy ), ( sa, sm ) ) =
  let
    ( ax, ay ) =
      ( cos (degrees sa), sin (degrees sa) )

    ( bx, by ) =
      ( cos (degrees ra), sin (degrees ra) )

    d =
      bx * ay - by * ax

    t1 =
      (ay * sx - ay * rx - ax * sy + ax * ry) / d

    t2 =
      -(-by * sx + by * rx + bx * sy - bx * ry) / d
  in
    if t2 < 0 || t2 > sm || t1 < 0 then
      Nothing
    else
      Just t2


world : World
world =
  [ ( ( -300, -300 ), ( 0, 600 ) )
  , ( ( 300, -300 ), ( 90, 600 ) )
  , ( ( -300, -300 ), ( 90, 600 ) )
  , ( ( 300, 300 ), ( 180, 600 ) )
  , ( ( 100, 100 ), ( -45, 50 ) )
  , ( ( 150, -100 ), ( 20, 120 ) )
  ]


main : Signal Element
main =
  Signal.map2 view Window.dimensions Mouse.position
