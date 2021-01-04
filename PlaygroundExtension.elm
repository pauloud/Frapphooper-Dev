module PlaygroundExtension exposing (..)
import PlaygroundOpen as P 
import Svg exposing(..)
import Svg.Attributes exposing (..)
import Html
import Html.Attributes as H 
import Browser
import Browser.Events as E
import Browser.Dom as Dom
import Task 


{-type Shape = Shape P.Shape |
type alias Custom {shape : P.Shape, attributes : List (Attribute msg), defs : List (Node msg) }

shape = Shape-}
render : P.Screen -> List (Html.Html msg) -> Svg msg
render screen svgs =
  let
    w = String.fromFloat screen.width
    h = String.fromFloat screen.height
    x = String.fromFloat screen.left
    y = String.fromFloat screen.bottom
  in
  svg
    [ viewBox (x ++ " " ++ y ++ " " ++ w ++ " " ++ h)
    {-, H.style "position" "fixed"
    , H.style "top" "0"
    , H.style "left" "0"
    , width "100%"
    , height "100%"
    -}]
    svgs

type alias Point2D = {x:Float,y:Float}
type alias Point2DTuple = (Float,Float)
unpoint : Point2D  ->  (Float -> Float -> a ) -> a
unpoint point f = f point.x point.y
point2DTuple : Point2D -> Point2DTuple
point2DTuple point2D = (point2D.x,point2D.y)
d2 : Point2D -> Point2D -> Float
d2 point0 point1 = sqrt ( (point0.x-point1.x) ^ 2 + (point0.y-point1.y) ^ 2)

withClipPath : String -> Svg msg -> (List (Attribute msg) -> List (Svg msg) -> Svg msg) -> List (Attribute msg) -> List (Svg msg)
withClipPath name path nodeFunction attributes = [defs [] [Svg.clipPath [id name][path]],
                                                 nodeFunction (attributes ++ [Svg.Attributes.clipPath (String.append "#" name)]) [] ]
                                                   
  --SVG shortcuts
ellipse : Float -> Float -> Float -> Float -> List ( Attribute msg) -> List (Svg msg) -> Svg msg
ellipse x y dx dy attr = Svg.ellipse ([cx (String.fromFloat x), cy (String.fromFloat y), rx (String.fromFloat (dx/2)), ry (String.fromFloat (dy/2))] ++ attr)
{-image : String -> Float -> Float -> Float -> Float -> List ( Attribute msg) -> List (Svg msg) -> Svg msg
image link x y dx dy = -}
--game : String -> (P.Computer -> memory -> List (Html.Html msg)) -> (P.Computer -> memory -> memory) -> memory -> Program () (P.Game memory) msg
game title viewMemory updateMemory initialMemory = 
  let
    init () =
      ( P.Game E.Visible initialMemory P.initialComputer
      , Task.perform P.GotViewport Dom.getViewport
      )

    view (P.Game _ memory computer) =
      { title = title
      , body = [ render computer.screen (viewMemory computer memory) ]
      }

    update msg model =
      ( P.gameUpdate updateMemory msg model
      , Cmd.none
      )

    subscriptions (P.Game visibility _ _) =
      case visibility of
        E.Hidden ->
          E.onVisibilityChange P.VisibilityChanged

        E.Visible ->
          P.gameSubscriptions
  in
  Browser.document
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }