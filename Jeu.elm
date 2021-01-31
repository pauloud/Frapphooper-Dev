module Jeu exposing (..)

import PlaygroundOpen as P
import PlaygroundExtension as X 
import List.Extra as LE
import Maybe.Extra as ME
import Svg exposing (..)
import Svg.Attributes exposing (..)
--import Random







-- Parametres globaux
ratioTaupes = 3/4
tailleTaupes = 30
acmeeTaupe = 60
diametreTrous = 75
formeTaupiniere = {c = 250, oX = 0, oY = 0 }
dxTrou trou = pX diametreTrous trou.y
dyTrou trou = pX diametreTrous (trou.y * 2)



--Perspective
fenetreProjection = {oX = 0, oY = 0, longueur = 1200, hauteur = 800}
deformationHauteur = 0.5
deformationLargeur = 0.25
perteLargeur = 0.1
perteHauteur = 0.1
pente = 1
pX o a = o*(fenetreProjection.hauteur/2 - deformationLargeur *  a) / (fenetreProjection.hauteur/2)
pY c a = a + c *(fenetreProjection.longueur/2 - a * deformationHauteur) / (fenetreProjection.longueur/2) 
projection :(Float,Float,Float) -> X.Point2D
projection (o,a,c) = { x = pX o a , y = pY c a }
projetterTrou trou = let centre = projection (trou.x + formeTaupiniere.oX, trou.y + formeTaupiniere.oY,0) 
                        in P.move centre.x centre.y (P.oval P.black (dxTrou trou) (dyTrou trou) )

position3DTaupe : Trou -> Taupe -> (Float,Float,Float)
position3DTaupe trou  taupe = (trou.x,trou.y, taupe.hauteur / 60 * tailleTaupes)
positionTaupe : Trou -> Taupe -> X.Point2D
positionTaupe  trou taupe =  projection   (position3DTaupe trou taupe)

--VIEW
type alias IdTrou = {trou : Trou, id : String}
view : P.Computer -> Memory -> List (Svg msg)
view computer memory =
  let
      sol =  let c = formeTaupiniere.c 
                 (oX,oY) = (formeTaupiniere.oX, formeTaupiniere.oY)
                  in 
                     [P.renderShape <| 
                     P.polygon P.lightBrown 
                     ([(-c + oX, -c + oY),(-c + oX,c + oY),(c + oX ,c + oY ),(c + oX ,-c + oY)] 
                     |> List.map (\(x,y) -> projection (y,x,0) |> X.point2DTuple)) 
                      ]
      
      centreTrou trou = projection (trou.x,trou.y,0)
      diametreX trou = pX diametreTrous trou.y
      diametreY trou = pX diametreTrous (trou.y * 2)        
      taupes = 
       let
        idTrousRec : Int -> List Trou -> List IdTrou -> List IdTrou
        idTrousRec n listeTrous retour = case listeTrous of
          [] -> retour
          (trou :: trouS) -> idTrousRec (n + 1) trouS ({trou = trou, id = String.fromInt n } :: retour)
        showTaupe : IdTrou -> Maybe (List (Svg msg))
        showTaupe idTrou = let trou = idTrou.trou in case trou.taupe of
          Nothing -> Nothing 
          Just taupe -> [xlinkHref "tete_hooper.png", 
                       x (String.fromFloat <| .x <| positionTaupe  trou taupe) , 
                       y (String.fromFloat <| .y <| positionTaupe  trou taupe), 
                       width (String.fromFloat <| pX (tailleTaupes*ratioTaupes) trou.x),
                       height (String.fromFloat <| pY  tailleTaupes trou.y)]
                       |> X.withClipPath 
                           (String.append "cpTaupeTrou" idTrou.id) 
                           (rect 
                               [x <| String.fromFloat <|  (positionTaupe trou taupe).x - diametreX trou / 2, 
                                y <| String.fromFloat <| (positionTaupe  trou taupe).y - diametreY trou / 2,
                                width <| String.fromFloat (pX (tailleTaupes*ratioTaupes) trou.x),
                                height <| String.fromFloat (pY  tailleTaupes trou.y)] [])
                           image
                       |> Just 
          in List.concat <| ME.values <|  List.map showTaupe (idTrousRec  1 memory.taupiniere.trous [])
        
      trous = List.map (\trou -> P.renderShape <| projetterTrou trou) memory.taupiniere.trous
      --P.move (centreTrou trou).x (centreTrou trou).y (P.oval P.black (diametreX trou) (diametreY trou)
        in List.concat [sol,trous,taupes] 

-- MAIN

{--}
memory0 = let trou0 x y = {taupe = Just (Taupe (Mobile 1) Skin1 0),x = x , y = y}
              (dX,dY) = (100,100)
              in {score = 0, taupiniere = Taupiniere 
                   [trou0 (-dX) (-dY), trou0 0 (-dY), trou0 dX (-dY),
                    trou0 (-dX) 0, trou0 0 0, trou0 dX 0,
                    trou0 (-dX) dY, trou0 0 dY, trou0 dX dY
                    ] 0 }

main : Program () (P.Game Memory) P.Msg                
main = X.game "pauloud" view update memory0 
-- MEMORY
type alias Memory =
  { score: Int,
  taupiniere: Taupiniere}
score1 = \memory -> {memory | score = memory.score + 1}
type VitesseTaupe = MonteFloat
type EtatTaupe = Mobile Float | Mort Int
type SkinTaupe = Skin1 | Skin2
type alias Taupe = {etat:EtatTaupe,skin :SkinTaupe,hauteur : Hauteur}
type alias Hauteur = Float
type alias Trou = {taupe : Maybe Taupe, x:Float, y:Float}
type alias Taupiniere = {trous : List Trou, nbTaupes : Int }


--UPDATE
persistanceCadavres = 90
update : P.Computer -> Memory -> Memory 
update computer memory =

  let 
    --continuation , updateTaupiniere , updateScore : Memory -> Memory  
    continuation = identity

    updateTaupiniere memory1 = 
      let trous1 : List Trou
          trous1 = 
            let 
              updateTrou : Trou -> Trou
              updateTrou trou  = 
                case trou.taupe of
                  Nothing -> trou
                  Just taupe ->  let 
                                    updatedTaupe = case taupe.etat of
                                              Mort t -> if t > persistanceCadavres 
                                                        then Nothing 
                                                        else Just {taupe | etat = Mort (t + 1)}
                                              Mobile v -> if taupe.hauteur > acmeeTaupe 
                                                          then Just {taupe | etat = Mobile (v * -1), hauteur = taupe.hauteur - v}
                                                          else  if taupe.hauteur >= 0
                                                                then Just {taupe | etat = Mobile v, hauteur = taupe.hauteur + v}
                                                                else Nothing
                                    in {trou | taupe = updatedTaupe}
            in List.map updateTrou memory.taupiniere.trous                         
          in {memory1 | taupiniere = { nbTaupes = LE.count  (.taupe >> ((/=) Nothing) ) trous1, trous = trous1}}

  

    updateScore memory1 = 
      let checkTrous : X.Point2D -> (List Trou, Memory -> Memory)
          checkTrous clickPoint = let checkTrou point trou = 
                                         case trou.taupe of 
                                             Just taupe1 -> if X.d2 point (positionTaupe trou taupe1) <= 20 
                                                           then ({trou | taupe = Just {taupe1 | etat = Mort 0}} , score1) 
                                                           else (trou,identity)
                                             Nothing -> (trou, identity)
                                        in List.foldr 
                                            (\(trou,update1) (trous,updates) -> (trou::trous,update1 >> updates)) 
                                            ([], identity) 
                                            (memory.taupiniere.trous |> List.map (checkTrou clickPoint)) 
 
      in case computer.mouse.click of
        True -> let 
                  (trous1,updatedScore) = checkTrous {x = computer.mouse.x , y = computer.mouse.y}
                  taupiniere1 = memory1.taupiniere
                  in updatedScore {memory1 | taupiniere = {taupiniere1 | trous = trous1}}
        _ -> memory1
    in memory |> continuation << updateTaupiniere << updateScore







