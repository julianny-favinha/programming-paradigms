-- para usar o sort
import Data.List
import Prelude

-- tipo Point tem o nome do vertice e sua lista de coordenadas
data Point = Point String [Float] deriving Show

-- tipo Edge tem o nome dos dois vertices e a distancia entre eles
data Edge = Edge Float String String deriving (Show, Ord, Eq)

data Graph = Graph [Point] [Edge] deriving (Show)

-- cria lista de Edges a partir da distancia entre Points
createEdges :: [Point] -> [Edge]
createEdges listPoints = [distancePoints (Point p1 coord1) (Point p2 coord2) | (Point p1 coord1) <- listPoints, (Point p2 coord2) <- listPoints, p1 < p2]

-- calcula distancia entre Points
distancePoints :: Point -> Point -> Edge
distancePoints (Point p1 coord1) (Point p2 coord2) = Edge (sqrt $ sum $ zipWith (\ f1 f2 -> (f1 - f2)**2) coord1 coord2) p1 p2

-- converte uma linha do input em Point
convert :: [String] -> Point
convert line = do 
    let
      nome = head line
      coords = map read (tail line)
    (Point nome coords)

-- remove os n primeiros elementos de uma lista
drop' :: Int -> [a] -> [a]
drop' n xs = [ v | (_,v)  <- filter (\(x,_) -> x > n) $ zip [1..] xs ]

-- ************* MAIN *************
main = do 
    k <- getLine
    c <- getContents
    let 
        l = lines c 
        w = map words l
        points = map convert w
        
    print $ drop' ((read k)-1) (reverse $ sort $ createEdges $ points)
    --print $ kruskal (sort $ createEdges $ points)
