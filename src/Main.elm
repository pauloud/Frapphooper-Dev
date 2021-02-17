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
import Game.Ressources as R 

--VIEW
c = formeTaupiniere.c 
oX = formeTaupiniere.oX
oY = formeTaupiniere.oY
 
infosDebutants = [image [xlinkHref "tete_hooper.png"
                         ,transform <| P.renderTransform -100 200 0 0.3] []
                  ,image [xlinkHref "issou.png"
                         ,transform <| P.renderTransform -100 0 0 1] []

                  , text_ [transform <| P.renderTransform 80 100 0 2.2] [text " + 1"] 
                  , text_ [transform <| P.renderTransform 80 (-100) 0 2.2, fill "#FF0000"] [text " - 3"] ]
                                
                                
                                
                                           
sol =   [P.renderShape 
         <| P.polygon P.lightBrown 
                     ([(-c + oX, -c + oY),(-c + oX,c + oY),(c + oX ,c + oY ),(c + oX ,-c + oY)] 
         |> List.map (\(x,y) -> projection (y,x,0) |> X.point2DTuple)) 
          ]
bouton texte =  [ rect [x <| String.fromInt -150, y <| String.fromInt (-350), width "300", height "75", fill "#00C0FF", Events.onClick Start] [], text_ [transform <| P.renderTransform (-100) 300 0 2.2,  Events.onClick Start] [text texte]]
viewPartie :  Partie -> List (Svg Message)
viewPartie memory =
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
          Just taupe -> if taupe.hauteur > 5 then Just <|
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
                                                            String.fromFloat  <| .x <| positionTaupe trou taupe,
                                                            ",",
                                                            String.fromFloat <| .y <| positionTaupe trou taupe,")"]),
                                width <| String.fromFloat (pX (tailleTaupes*ratioTaupes) trou.y),
                                height <| String.fromFloat (pX  tailleTaupes (trou.y*2))]
                                ++ case taupe.etat of
                                  Mobile _ -> [Events.onClick <| Kill trou.id taupe.typeTaupe]
                                  Mort _ -> [])
                                
                                
                               []] else Nothing

          in List.concat <| ME.values <|  List.map showTaupe (memory.taupiniere)
        
      trous = let projetterTrou trou = P.move (centreTrou trou).x (centreTrou trou).y (P.oval P.black (diametreX trou) (diametreY trou))
                                        in List.map (\trou -> P.renderShape <| projetterTrou trou) memory.taupiniere
      --P.move (centreTrou trou).x (centreTrou trou).y (P.oval P.black (diametreX trou) (diametreY trou)
        in List.concat <| [sol,trous,taupes,score,temps] 

view model = case model of 
  Jeu partie -> viewPartie partie
  Debut -> List.concat [sol,bouton "Démarrer",infosDebutants]
  Fin partie -> List.concat [viewPartie partie, bouton "Rejouer"]


-- INIT

{--}
type AppModel = Debut | Jeu Partie | Fin Partie 
{-
 viewApp appModel = case appModel of
  Debut -> [P.words P.darkGreen (String.append "Demarrer" (String.fromInt memory.score))
              |> (P.scale 2.2)
              |> P.move (formeTaupiniere.oX + formeTaupiniere.c/2) (formeTaupiniere.oY - formeTaupiniere.c/2)
              |> P.renderShape] -}


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
                  taupiniere =  idTrousRec 1 [ trou0 (-ecartX) (-ecartY),trou0 0 (-ecartY), trou0 ecartX (-ecartY),
                                           trou0 (-ecartX) 0.0, trou0 0 0.0, trou0 ecartX 0.0,
                                           trou0 (-ecartX) ecartY, trou0 0.0 ecartY, trou0 ecartX ecartY] []}


taupeGenerator tempsG typeG = Random.map2 (\temps typeTaupe -> (toFloat temps, {taupe0 | typeTaupe = typeTaupe})) 
                                            tempsG
                                            typeG
taupesGenerator : Random.Generator (List (List (Float,Taupe) ) )
taupesGenerator =  Random.list 9 <| Random.map (List.sortBy <| (round << (/) 10) << Tuple.first) <| Random.map2 (++) 
                                                               (Random.list 8 <| taupeGenerator (Random.int 15 33) 
                                                                              <| Random.weighted (75,Gentil) [(25,Mechant)]) 
                                                               (Random.list 8 <| taupeGenerator (Random.int 4 14) 
                                                                              <| Random.weighted (60,Gentil) [(40,Mechant)]) 
              
                                                          
generationTaupes = Random.generate Init <| Random.map 
                                          (List.map2 (\trou file -> {trou|file= file}) memory0.taupiniere) 
                                          taupesGenerator                                                          
init () =  (Debut, Cmd.none)
update message model = case model of
  Jeu partie ->  (updateMemory message partie |> if partie.tempsRestant <= 0 then Fin else Jeu
                  , Cmd.none) 
  _ -> case message of
      Start -> ( Jeu <| Partie 0 taupiniereVide 30,generationTaupes)
      _ -> (model, Cmd.none) 


   
--MAIN

main = Browser.document {init = init,
                         view =  \appModel -> {title = "frapphooper",
                                             body = [ (Lazy.lazy  <| svg [viewBox "-650 -400  1200 1200"]) (view appModel)]},
                         update = update,

                         subscriptions = \memory-> Sub.batch [E.onAnimationFrameDelta Tick]}
                                                          