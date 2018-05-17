import Data.List
import Data.Ord

-- tipo Point tem o nome do vertice e sua lista de coordenadas
data Point = Point String [Float]

-- tipo Edge tem o nome dos dois vertices e a distancia entre eles
data Edge = Edge {distance :: Float, v1 :: String, v2 :: String} deriving (Show)

-- TODO: ordenar por distancia, crescente
main = print $ createEdges [(Point "a" [1.0,2.0]), (Point "b" [3.0,4.0]), (Point "c" [5.0,6.0])]

createEdges :: [Point] -> [Edge]
createEdges listPoints = [distancePoints (Point p1 coord1) (Point p2 coord2) | (Point p1 coord1) <- listPoints, (Point p2 coord2) <- listPoints, p1 < p2]

distancePoints :: Point -> Point -> Edge
distancePoints (Point p1 coord1) (Point p2 coord2) = Edge (sqrt $ sum $ zipWith (subtractSquare) coord1 coord2) p1 p2

subtractSquare :: Float -> Float -> Float
subtractSquare f1 f2 = (f1 - f2) ** 2