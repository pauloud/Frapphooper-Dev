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
import Html.Lazy as Lazy


{-type Shape = Shape P.Shape |
type alias Custom {shape : P.Shape, attributes : List (Attribute msg), defs : List (Node msg) }

shape = Shape-}
--render : P.Screen -> List (Html.Html msg) -> Svg msg
tail list = case list of 
             [] -> []
             (x::xs) -> xs
toCmd message = Task.perform identity (Task.succeed message)
toScreen : Float -> Float -> P.Screen
toScreen width height =
  { width = width
  , height = height
  , top = height / 2
  , left = -width / 2
  , right = width / 2
  , bottom = -height / 2
  }
render screen svgs =
  let
    w = String.fromFloat screen.width
    h = String.fromFloat screen.height
    x = String.fromFloat screen.left
    y = String.fromFloat (screen.bottom + 200)
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


type alias HtmlB msg = {node : Html.Html msg, attributes : List (Attribute msg), children : List (Html.Html msg)}

withClipPath : String -> Svg msg -> (List (Attribute msg) -> List (Svg msg) -> Svg msg) -> List (Attribute msg) -> List (Svg msg) -> List (Svg msg)
withClipPath name path nodeFunction attributes children = [defs [] [Svg.clipPath [id name][path]],
                                                 (Lazy.lazy <| nodeFunction (attributes ++ [Svg.Attributes.clipPath (String.append "#" name)])) children ]
                                                   
  --SVG shortcuts
ellipse : Float -> Float -> Float -> Float -> List ( Attribute msg) -> List (Svg msg) -> Svg msg
ellipse x y dx dy attr = Svg.ellipse ([cx (String.fromFloat x), cy (String.fromFloat y), rx (String.fromFloat (dx/2)), ry (String.fromFloat (dy/2))] ++ attr)
{-image : String -> Float -> Float -> Float -> Float -> List ( Attribute msg) -> List (Svg msg) -> Svg msg
image link x y dx dy = -}



--type Msg msg = GameMsg msg | WindowMsg P.Msg
game : String -> (P.Computer -> memory -> List (Html.Html P.Msg)) -> (P.Computer -> memory -> memory) -> memory -> Program () (P.Game memory) P.Msg
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
{-gameSubscriptions : Sub P.Msg
gameSubscriptions =
  Sub.batch
    [ E.onResize Resized
    , E.onKeyUp (D.map (KeyChanged False) (D.field "key" D.string))
    , E.onKeyDown (D.map (KeyChanged True) (D.field "key" D.string))
    , E.onAnimationFrame Tick
    , E.onVisibilityChange VisibilityChanged
    , E.onClick (D.succeed MouseClick)
    , E.onMouseDown (D.succeed (MouseButton True))
    , E.onMouseUp (D.succeed (MouseButton False))
    , E.onMouseMove (D.map2 MouseMove (D.field "pageX" D.float) (D.field "pageY" D.float))
    ]
gameUpdate : (P.Computer -> memory -> memory) -> P.Msg -> P.Game memory -> P.Game memory
gameUpdate updateMemory msg (Game vis memory computer) =
  case msg of
    Tick time ->
      Game vis (updateMemory computer memory) <|
        if computer.mouse.click
        then { computer | time = Time time, mouse = mouseClick False computer.mouse }
        else { computer | time = Time time }

    GotViewport {viewport} ->
      Game vis memory { computer | screen = toScreen viewport.width viewport.height }

    Resized w h ->
      Game vis memory { computer | screen = toScreen (toFloat w) (toFloat h) }

    KeyChanged isDown key ->
      Game vis memory { computer | keyboard = updateKeyboard isDown key computer.keyboard }

    MouseMove pageX pageY ->
      let
        x = computer.screen.left + pageX
        y = computer.screen.top - pageY
      in
      Game vis memory { computer | mouse = mouseMove x y computer.mouse }

    MouseClick ->
      Game vis memory { computer | mouse = mouseClick True computer.mouse }

    MouseButton isDown ->
      Game vis memory { computer | mouse = mouseDown isDown computer.mouse }

    VisibilityChanged visibility ->
      Game visibility memory
        { computer
            | keyboard = emptyKeyboard
            , mouse = Mouse computer.mouse.x computer.mouse.y False False
        }-}