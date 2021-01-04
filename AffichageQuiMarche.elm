module AffichageQuiMarche exposing (..)

import Playground exposing (..)
import List.Extra as LE
--import Random





d2 (x,y) (x1,y1) = sqrt ( (x-x1) ^ 2 + (y-y1) ^ 2)


-- MAIN
main =
  let trou0 x y = Trou Nothing x y
      dX = 200
      dY = 200
      in game view update {score = 0, taupiniere = Taupiniere 
        [trou0 (-dX) (-dY), trou0 0 (-dY), trou0 dX (-dY),
         trou0 (-dX) 0, trou0 0 0, trou0 dX 0,
         trou0 (-dX) dY, trou0 0 dY, trou0 dX dY
          ] 0 }
-- MEMORY
type alias Memory =
  { score: Int,
  taupiniere: Taupiniere}
score1 = \memory -> {memory | score = memory.score + 1}
type EtatTaupe = Mobile Int | Mort Int
type SkinTaupe = Skin1 | Skin2
type alias Taupe = {etat:EtatTaupe,skin :SkinTaupe,hauteur : Hauteur}
type alias Hauteur = Int
type alias Trou = {taupe : Maybe Taupe, x:Float, y:Float}
type alias Taupiniere = {trous : List Trou, nbTaupes : Int }


--UPDATE
persistanceCadavres = 40
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
                                              Mort t -> if t > persistanceCadavres then Nothing else Just {taupe | etat = Mort (t + 1)}
                                              Mobile v -> Just {taupe | hauteur = taupe.hauteur + v }
                                    in {trou | taupe = updatedTaupe}
            in List.map updateTrou memory.taupiniere.trous                         
          in {memory1 | taupiniere = { nbTaupes = LE.count  (.taupe >> ((==) Nothing) >> not ) trous1, trous = trous1}}

  

    updateScore memory1 = 
      let checkTrous : (Float,Float) -> (List Trou, Memory -> Memory)
          checkTrous (x,y) = let 
                                checkTrou (x1,y1) trou = case trou.taupe of 
                                  Just taupe1 -> 
                                    if (d2 (x1,y1) (positionTaupe computer trou taupe1)) <= 20 then ({trou | taupe = Just {taupe1 | etat = Mort 0}} , score1) else (trou,identity)
                                  Nothing -> 
                                    (trou, identity)
                              in List.foldr (\(trou,update1) (trous,updates) -> (trou::trous,update1 >> updates)) ([], identity) (memory.taupiniere.trous |> List.map (checkTrou (x,y)) ) 

      in case computer.mouse.click of
        True -> let 
                  (trous1,updatedScore) = checkTrous (computer.mouse.x , computer.mouse.y) 
                  taupiniere1 = memory1.taupiniere
                  in updatedScore {memory1 | taupiniere = {taupiniere1 | trous = trous1}}
        _ -> memory1
    in memory |> continuation << updateTaupiniere << updateScore

--VIEW
tailleTaupes = 10



position3DTaupe trou taupe = (trou.x,trou.y,toFloat taupe.hauteur / 60 * tailleTaupes)
positionTaupe : Computer -> Trou -> Taupe -> (Float,Float)
positionTaupe computer trou taupe =  projection (position3DTaupe trou taupe)

fenetreProjection = {oX = 0, oY = 0, longueur = 1200, hauteur = 800}
deformationHauteur = 0.5
deformationLargeur = 0.25
formeTaupiniere = {c = 600, oX = 0, oY = 0 }


projection (o,a,c) = (pX o a , pY c a )
pX o a = o*(fenetreProjection.hauteur/2 - deformationLargeur *  a) / (fenetreProjection.hauteur/2)
pY c a = a + c *(fenetreProjection.longueur/2 - a * deformationHauteur) / (fenetreProjection.longueur/2) 

diametreTrous = 75
dxTrou trou = pX diametreTrous trou.y
dyTrou trou = pX diametreTrous (trou.y * 2)

projetterTrou trou = let (x,y) = projection (trou.x + formeTaupiniere.oX, trou.y + formeTaupiniere.oY,0) 
                        in move x y (oval black (dxTrou trou) (dyTrou trou) )


view computer memory =
    let c = formeTaupiniere.c / 2
        (oX,oY) = (formeTaupiniere.oX, formeTaupiniere.oY)
        sol = [polygon lightBrown ([(-c + oX, -c + oY),(-c + oX,c + oY),(c + oX ,c + oY ),(c + oX ,-c + oY)] |> List.map (\(x,y) -> projection (y,x,0))) ]
        trous = List.map projetterTrou memory.taupiniere.trous
     in List.concat [sol,trous]





