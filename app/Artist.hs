module Artist where

import UdGraphic ( Angle, Comanda(..), Distancia )
import Test.QuickCheck ()
import Debug.Trace ()

-- Problema 1

separa :: Comanda -> [Comanda]
separa Para = []
separa (Avança x) = [Avança x]
separa (Gira x) = [Gira x]
separa (c1 :#: c2) = separa c1 ++ separa c2

-- >>> separa  (Avança 3 :#: Gira 4 :#: Avança 7 :#: Para)
-- [Avança 3.0,Gira 4.0,Avança 7.0]

-- >>> separa (((Para :#: Avança 3) :#: Gira 4) :#: Avança 7)
-- [Avança 3.0,Gira 4.0,Avança 7.0]


-- Problema 2

ajunta :: [Comanda] -> Comanda
ajunta = foldr (:#:) Para

-- >>> ajunta [Avança 3, Gira 4, Avança 7]
-- Avança 3.0 :#: (Gira 4.0 :#: (Avança 7.0 :#: Para))


-- Problema 3

-- prop_equivalent = undefined
prop_equivalent :: Comanda -> Comanda -> Bool
prop_equivalent comanda1 comanda2 = equivalent (separa comanda1) (separa comanda2)
  where
    equivalent :: [Comanda] -> [Comanda] -> Bool
    equivalent [] [] = True
    equivalent (c1:cs1) (c2:cs2) = c1 `equiv` c2 && equivalent cs1 cs2
    equivalent _ _ = False

    equiv :: Comanda -> Comanda -> Bool
    equiv Para Para = True -- potser no cal
    equiv (Avança x1) (Avança x2) = x1 == x2
    equiv (Gira x1) (Gira x2) = x1 == x2 
    equiv _ _ = False

-- >>> prop_equivalent (Avança 3.0 :#: (Gira 4.0 :#: (Avança 7.0 :#: Para))) (Avança 3.0 :#: (Gira 4.0 :#: (Avança 7.0 :#: Para))) 
-- True

-- >>> prop_equivalent (Avança 4.0 :#: (Gira 4.0 :#: (Avança 7.0 :#: Para))) (Avança 3.0 :#: (Gira 4.0 :#: (Avança 7.0 :#: Para))) 
-- False


-- prop_split_join = undefined
prop_split_join :: Comanda -> Bool
prop_split_join c = prop_equivalent c (ajunta (separa c))

-- >>> prop_split_join (Avança 3.0 :#: (Gira 4.0 :#: (Avança 7.0 :#: Para)))
-- True



-- prop_split = undefined
prop_split :: Comanda -> Bool
prop_split c = prop_split_list (separa c)
    where 
        prop_split_list [] = True
        prop_split_list (c:cs) = case c of
                                    Para -> False
                                    _ :#: _ -> False
                                    _ -> prop_split_list cs

-- >>> separa (((Para :#: Avança 3) :#: Gira 4) :#: Avança 7)
-- >>> prop_split (((Para :#: Avança 3) :#: Gira 4) :#: Avança 7)
-- [Avança 3.0,Gira 4.0,Avança 7.0]
-- True

-- Problema 4

copia :: Int -> Comanda -> Comanda
copia n comanda
  | n <= 0 = Para
  | n == 1 = comanda
  | otherwise = comanda :#: copia (n-1) comanda

-- >>> copia 3 (Avança 10 :#: Gira 120)
-- (Avança 10.0 :#: Gira 120.0) :#: ((Avança 10.0 :#: Gira 120.0) :#: (Avança 10.0 :#: Gira 120.0))

-- >>> copia 0 (Avança 10 :#: Gira 120)
-- Para

-- Problema 5

pentagon :: Distancia -> Comanda
pentagon d = copia 5 (Avança d :#: Gira 72)

-- >>> pentagon 50.0
-- (Avança 50.0 :#: Gira 72.0) :#: ((Avança 50.0 :#: Gira 72.0) :#: ((Avança 50.0 :#: Gira 72.0) :#: ((Avança 50.0 :#: Gira 72.0) :#: (Avança 50.0 :#: Gira 72.0))))


-- Problema 6

poligon :: Distancia -> Int -> Angle -> Comanda
poligon dist numSides angle = copia numSides (Avança dist :#: Gira angle)

-- >>> poligon 50 5 72
-- (Avança 50.0 :#: Gira 72.0) :#: ((Avança 50.0 :#: Gira 72.0) :#: ((Avança 50.0 :#: Gira 72.0) :#: ((Avança 50.0 :#: Gira 72.0) :#: (Avança 50.0 :#: Gira 72.0))))

prop_poligon_pentagon ::  Distancia -> Int -> Angle -> Bool
prop_poligon_pentagon dist num angle = prop_equivalent (pentagon dist) (poligon dist num angle)

-- >>> prop_poligon_pentagon 50 5 72
-- True


-- Problema 7

espiral :: Distancia -> Int -> Distancia -> Angle -> Comanda
espiral distInicial numVoltes distIncremento angle = construirEspiral distInicial numVoltes distIncremento angle Para
  where
    construirEspiral _ 0 _ _ comandaFinal = comandaFinal

    construirEspiral distActual voltes incremento angulo comandaFinal =
      let comanda = Avança distActual :#: Gira angulo
          nuevaDistancia = distActual + incremento
          nuevaComandaFinal = comandaFinal :#: comanda
      in construirEspiral nuevaDistancia (voltes - 1) incremento angulo nuevaComandaFinal

-- >>> espiral 30 4 5 30
-- (((Para :#: (Avança 30.0 :#: Gira 30.0)) :#: (Avança 35.0 :#: Gira 30.0)) :#: (Avança 40.0 :#: Gira 30.0)) :#: (Avança 45.0 :#: Gira 30.0)



-- Problema 9

optimitza :: Comanda -> Comanda
optimitza = undefined

-- Problema 10

triangle :: Int -> Comanda
triangle = undefined

-- Problema 11

fulla :: Int -> Comanda
fulla = undefined

-- Problema 12

hilbert :: Int -> Comanda
hilbert = undefined

-- Problema 13

fletxa :: Int -> Comanda
fletxa = undefined

-- Problema 14

branca :: Int -> Comanda
branca = undefined
