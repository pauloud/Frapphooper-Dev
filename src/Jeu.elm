module Jeu exposing (..)

import PlaygroundOpen as P
import PlaygroundExtension as X 
import List.Extra as LE
import Maybe.Extra as ME
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Browser
import Browser.Events as E
import Svg.Events as Events
import Time
import Json.Decode as D








-- Parametres globaux
ratioTaupes = 3/4
tailleTaupes = 70
acmeeTaupe = 90
diametreTrous = 90
formeTaupiniere = {c = 300, oX = 0, oY = 0 }
diametreX trou = pX diametreTrous trou.y
diametreY trou = pX diametreTrous (trou.y * 2)
centreTrou trou =  projection (trou.x + formeTaupiniere.oX, trou.y + formeTaupiniere.oY,0) 

                                           




--Perspective
fenetreProjection = {oX = 0, oY = 0, longueur = 800, hauteur = 800}
deformationHauteur = 0.5
deformationLargeur = 0.25
pente = 1
pX o a = o*(fenetreProjection.hauteur/2 - deformationLargeur *  a) / (fenetreProjection.hauteur/2)
pY c a = a + c *(fenetreProjection.longueur/2 - a * deformationHauteur) / (fenetreProjection.longueur/2) 
projection :(Float,Float,Float) -> X.Point2D
projection (o,a,c) = { x = pX o a , y = pY c a }



positionTaupe : Trou -> Taupe -> X.Point2D
positionTaupe  trou taupe = projection ((centreTrou trou).x - (diametreX trou)/4  , 
                                       -(centreTrou trou).y - (diametreY trou)/4  ,
                                        -taupe.hauteur*tailleTaupes/acmeeTaupe )
{- projectionTaupe trou taupe = {position = projection (position3DTaupe trou taupe), 
                              hauteur = py tailleTaupes trou.y} -}
                            
                                  







                          
-- MODEL/MEMORY
type alias IdTrou = String 
type Message = Kill IdTrou | Tick Float  | Init (List Trou) --| NouvelleTaupe Int 
type alias Partie =
  { score: Int,
    taupiniere: Taupiniere,
    tempsRestant: Float}
type VitesseTaupe = MonteFloat
type EtatTaupe = Mobile Float | Mort Int
type SkinTaupe = Skin1 | Skin2
type alias Taupe = {etat:EtatTaupe,skin :SkinTaupe,hauteur : Hauteur}
taupe0 = Taupe (Mobile 1) Skin1 0 
type alias Hauteur = Float
type alias Trou = {taupe : Maybe Taupe, file: List (Float,Taupe) , x:Float, y:Float, id:IdTrou}
type alias Taupiniere = {trous : List Trou}
taupiniereVide = Taupiniere []



--UPDATE
persistanceCadavres = 90
updateMemory : Message -> Partie -> Partie
updateMemory message memory = let 
                                  tempsEcoule = memory.tempsRestant <=0 
                                  partieTerminee = {memory|taupiniere = taupiniereVide}
                                  tuerTaupe id trou = case trou.taupe of
                                              Nothing -> trou
                                              Just taupe -> if trou.id == id 
                                                            then {trou | taupe = Just {taupe | etat = Mort 0}}
                                                            else trou 
                            
                                  suiteTaupe tempsRestant trou = case trou.taupe of
                                                        Nothing -> case LE.getAt 0 trou.file of
                                                                                     Nothing -> trou
                                                                                     Just (tempsMax,taupe) -> if tempsRestant < tempsMax then {trou | taupe = Just taupe
                                                                                                                                                    , file = X.tail trou.file}
                                                                                                                                         else trou
                                                                                     
                                                        Just taupe -> {trou | taupe = case taupe.etat of 
                                                                         Mort t -> if t > persistanceCadavres 
                                                                                 then Nothing
                                                                                 else Just {taupe | etat = Mort (t + 1)}
                                                                         Mobile v -> if taupe.hauteur > acmeeTaupe 
                                                                                   then Just {taupe | etat = Mobile (v * -1), hauteur = taupe.hauteur - v}
                                                                                   else  if taupe.hauteur >= 0
                                                                                         then Just {taupe | etat = Mobile v, hauteur = taupe.hauteur + v}
                                                                                         else Nothing}
                            {-nouvelleTaupe num trous = let ajouter taupe trou = case trou.taupe of
                                                                Nothing -> {trou|taupe = Just taupe}
                                                                Just _ -> {trou|file = taupe :: trou.file}
                                                      in LE.updateAt num (ajouter taupe0) trous-} 
                                                                      
                             in if tempsEcoule 
                                then partieTerminee 
                                else let taupiniere = memory.taupiniere
                                                    in case message of 
                                                      Kill idTrou -> {memory| 
                                                                      taupiniere = { taupiniere | trous = List.map (tuerTaupe idTrou) taupiniere.trous}, 
                                                                      score = memory.score + 1}
                                                      Tick deltaT -> {memory | taupiniere = {taupiniere | trous = List.map (suiteTaupe memory.tempsRestant)  taupiniere.trous},
                                                                                     tempsRestant = memory.tempsRestant-deltaT/1000}
                                                      Init trous -> {memory | taupiniere = Taupiniere trous}
                                                      {-NouvelleTaupe index -> {memory | taupiniere = {taupiniere | 
                                                                                  trous = nouvelleTaupe index taupiniere.trous,
                                                                                  nbTaupes = taupiniere.nbTaupes + 1}}-}









