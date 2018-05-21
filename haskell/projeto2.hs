-- para usar o sort
import Data.List
import Prelude

-- tipo Point tem o nome do vertice e sua lista de coordenadas
data Point = Point String [Float] deriving Show

-- tipo Edge tem o nome dos dois vertices e a distancia entre eles
data Edge = Edge Float String String deriving (Show, Ord, Eq)


createEdges :: [Point] -> [Edge]
createEdges listPoints = [distancePoints (Point p1 coord1) (Point p2 coord2) | (Point p1 coord1) <- listPoints, (Point p2 coord2) <- listPoints, p1 < p2]

distancePoints :: Point -> Point -> Edge
distancePoints (Point p1 coord1) (Point p2 coord2) = Edge (sqrt $ sum $ zipWith (subtractSquare) coord1 coord2) p1 p2

subtractSquare :: Float -> Float -> Float
subtractSquare f1 f2 = (f1 - f2) ** 2


-- convert single param to int
rFloat :: String -> Float
rFloat = read

rInt :: String -> Int
rInt = read

-- converting lines to int
convertAux :: [String] -> [Float]
convertAux y = map rFloat y

convert :: [String] -> Point
convert line = do 
    let
      h = head line -- head is not int
      r = tail line
      coords = convertAux r
    (Point h coords)

main = do 
    n <- getLine
    c <- getContents
    let 
        l = lines c 
        w = map words l
        points = map convert w
        
    print $ (sort $ createEdges $ points)
