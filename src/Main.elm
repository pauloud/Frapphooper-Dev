module Main exposing (..)
import Jeu exposing (..)
import PlaygroundOpen as P
import PlaygroundExtension as X 
import List.Extra as LE
import Maybe.Extra as ME
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Browser
import Browser.Events as E
import Svg.Events as Events
import Random
import Html.Lazy as Lazy 

--VIEW
sol =  let c = formeTaupiniere.c 
           (oX,oY) = (formeTaupiniere.oX, formeTaupiniere.oY)
                  in 
                     [P.renderShape <| 
                     P.polygon P.lightBrown 
                     ([(-c + oX, -c + oY),(-c + oX,c + oY),(c + oX ,c + oY ),(c + oX ,-c + oY)] 
                     |> List.map (\(x,y) -> projection (y,x,0) |> X.point2DTuple)) 
                      ]

view :  Partie -> List (Svg Message)
view memory =
  let 
      score = [P.words P.darkGreen (String.append "score : " (String.fromInt memory.score))
              |> (P.scale 2.2)
              |> P.move (formeTaupiniere.oX + formeTaupiniere.c/2 + 250) (formeTaupiniere.oY - formeTaupiniere.c/2)
              |> P.renderShape] 
      temps= [P.words P.red (String.append "tempsRestant : " (String.fromInt <| ceiling <| memory.tempsRestant))
              |> (P.scale 2)
              |> P.move (formeTaupiniere.oX + formeTaupiniere.c/2 + 230) (formeTaupiniere.oY - formeTaupiniere.c/2 + 50)
              |> P.renderShape] 
      
             
      taupes = 
       let
        showTaupe : Trou -> Maybe (List (Svg Message))
        showTaupe trou = case trou.taupe of
          Nothing -> Nothing 
          Just taupe -> Just <|
                           {-X.withClipPath
                             (String.append "cpTaupeTrou" trou.id) 
                               (rect 
                                  [ x <| String.fromFloat <|  (positionTaupe trou taupe).x - diametreX trou, 
                                    y <| String.fromFloat <| (positionTaupe  trou taupe).y - diametreY trou,
                                    width <| String.fromFloat (pX (tailleTaupes*ratioTaupes) trou.y),
                                    height <|String.fromFloat (pX  tailleTaupes (trou.y*2))] 
                                  [])-}
                                [image
                               ([xlinkHref (case (taupe.etat,taupe.typeTaupe) of 
                                             (Mobile _ ,Gentil) -> "tete_hooper.png"
                                             (Mobile _ , Mechant) -> "issou.png"
                                             (Mort _, Gentil) -> "bien.png"
                                             (Mort _, Mechant) -> "hoopWut.png"),
                                transform ( String.concat ["translate (", 
                                                            String.fromInt <<ceiling <| .x <| positionTaupe trou taupe,
                                                            ",",
                                                            String.fromFloat <| .y <| positionTaupe trou taupe,")"]),
                                width <| String.fromFloat (pX (tailleTaupes*ratioTaupes) trou.y),
                                height <| String.fromFloat (pX  tailleTaupes (trou.y*2))]
                                ++ case taupe.etat of
                                  Mobile _ -> [Events.onClick <| Kill trou.id taupe.typeTaupe]
                                  Mort _ -> [])
                                
                                
                               []]

          in List.concat <| ME.values <|  List.map showTaupe (memory.taupiniere.trous)
        
      trous = let projetterTrou trou = P.move (centreTrou trou).x (centreTrou trou).y (P.oval P.black (diametreX trou) (diametreY trou))
                                        in List.map (\trou -> P.renderShape <| projetterTrou trou) memory.taupiniere.trous
      --P.move (centreTrou trou).x (centreTrou trou).y (P.oval P.black (diametreX trou) (diametreY trou)
        in List.concat <| [sol,trous,taupes,score,temps] 

-- INIT

{--}
memory0 : Partie
memory0 = let trou0 x y = {taupe = Nothing, x = x , y = y, id = "", file = []}
              trou1 x y = let trou0xy = trou0 x y in {trou0xy | taupe = Just taupe0}
              (ecartX,ecartY) = (150,150)
              idTrousRec : Int -> List Trou -> List Trou -> List Trou 
              idTrousRec n listeTrous retour = case listeTrous of
                                                      [] -> retour
                                                      (trou :: trouS) -> idTrousRec (n + 1) trouS ({trou | id = String.fromInt n } :: retour)
              in {score = 0, 
                  tempsRestant = 30.0,
                  taupiniere = {
                                trous =    idTrousRec 1 [ trou0 (-ecartX) (-ecartY),trou0 0 (-ecartY), trou0 ecartX (-ecartY),
                                           trou0 (-ecartX) 0.0, trou0 0 0.0, trou0 ecartX 0.0,
                                           trou0 (-ecartX) ecartY, trou0 0.0 ecartY, trou0 ecartX ecartY] []}}
init () = let 
              taupeGenerator tempsG typeG = Random.map2 (\temps typeTaupe -> (toFloat temps, {taupe0 | typeTaupe = typeTaupe})) 
                                            tempsG
                                            typeG
    
              taupesGenerator : Random.Generator (List (List (Float,Taupe) ) )
              taupesGenerator =  Random.list 9 <| Random.map (List.sortBy <| (round << (/) 10) << Tuple.first) <| Random.map2 (++) 
                                                               (Random.list 8 <| taupeGenerator (Random.int 15 33) 
                                                                              <| Random.weighted (75,Gentil) [(25,Mechant)]) 
                                                               (Random.list 8 <| taupeGenerator (Random.int 14 4) 
                                                                              <| Random.weighted (60,Gentil) [(40,Mechant)]) 
               in (memory0, Random.generate Init <| Random.map 
                                                     (List.map2 (\trou file -> {trou|file= file}) memory0.taupiniere.trous) 
                                                     taupesGenerator)

   
--MAIN

main = Browser.document {init = init,
                         view =  \memory -> {title = "frapphooper",
                                             body = [ (Lazy.lazy  <| svg [viewBox "-650 -400  1200 1200"]) (view memory)]},
                         update = \message memory -> (updateMemory message memory, Cmd.none),

                         subscriptions = \memory-> Sub.batch [E.onAnimationFrameDelta Tick]}
                                                          