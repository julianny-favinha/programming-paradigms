-- para usar o sort
import Data.List

-- tipo Point tem o nome do vertice e sua lista de coordenadas
data Point = Point String [Float]

-- tipo Edge tem o nome dos dois vertices e a distancia entre eles
data Edge = Edge String String Float deriving Show

main = print $ criaArestas [(Point "a" [1.0,2.0]), (Point "b" [3.0,4.0]), (Point "c" [5.0,6.0])]

criaArestas :: [Point] -> [Edge]
criaArestas listaPontos = [distanciaEntrePontos (Point p1 coord1) (Point p2 coord2) | (Point p1 coord1) <- listaPontos, (Point p2 coord2) <- listaPontos, p1 < p2]

distanciaEntrePontos :: Point -> Point -> Edge
distanciaEntrePontos (Point p1 coord1) (Point p2 coord2) = Edge p1 p2 (sqrt $ sum $ zipWith (subtracaoAoQuadrado) coord1 coord2)

subtracaoAoQuadrado :: Float -> Float -> Float
subtracaoAoQuadrado a1 a2 = (a1 - a2) ** 2