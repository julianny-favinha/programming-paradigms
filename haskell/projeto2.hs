-- para usar o sort
import Data.List
import Prelude

-- tipo Vertex tem o nome do vertice e sua lista de coordenadas
data Vertex = Vertex String [Float] deriving Show

-- tipo Edge tem o nome dos dois vertices e a distancia entre eles
data Edge = Edge Float String String deriving (Show, Ord, Eq)

edgeSrc :: Edge -> String
edgeSrc (Edge _ a _) = a

edgeDest :: Edge -> String
edgeDest (Edge _ _ b) = b

edgeWeight :: Edge -> Float
edgeWeight (Edge d _ _) = d

data Graph = Graph [Vertex] [Edge] deriving (Show)

-- cria lista de Edges a partir da distancia entre Vertex
createEdges :: [Vertex] -> [Edge]
createEdges listVertex = [distanceVertex (Vertex p1 coord1) (Vertex p2 coord2) | (Vertex p1 coord1) <- listVertex, (Vertex p2 coord2) <- listVertex, p1 < p2]

-- calcula distancia entre Vertex
distanceVertex :: Vertex -> Vertex -> Edge
distanceVertex (Vertex p1 coord1) (Vertex p2 coord2) = Edge (sqrt $ sum $ zipWith (\ f1 f2 -> (f1 - f2)**2) coord1 coord2) p1 p2

-- converte uma linha do input em Vertex
convert :: [String] -> Vertex
convert line = do 
    let
      nome = head line
      coords = map read (tail line)
    (Vertex nome coords)

-- remove os n primeiros elementos de uma lista
drop' :: Int -> [a] -> [a]
drop' n xs = [v | (_,v) <- filter (\(x,_) -> x > n) $ zip [1..] xs]

kruskal :: [Edge] -> [Edge]
kruskal g = kruskalRec g [] [ (x, x) | x <- (points g)]

kruskalRec :: [Edge] -> [Edge] -> [(String, String)] -> [Edge]
kruskalRec [] progress sets = progress
kruskalRec (e:es) progress sets
    | joined (edgeSrc e) (edgeDest e) sets = kruskalRec es progress sets
    | otherwise = kruskalRec es (e : progress) (join (edgeSrc e) (edgeDest e) sets)

parent :: String -> [(String, String)] -> String
parent elem sets
    |fst match == snd match = fst match
    |otherwise = parent (snd match) sets
    where
        match = head ([ (x, y) |(x, y) <- sets, x == elem]) 

joined :: String -> String -> [(String, String)] -> Bool
joined x y sets = (parent x sets) == (parent y sets)

join :: String -> String -> [(String, String)] -> [(String, String)]
join x y sets
    | joined x y sets = sets
    | otherwise = newY : rest
    where
        newY = (fst oldY, x)
        oldY = head [(a, b) | (a, b) <- sets, a==(parent y sets)]
        rest = [(a, b) | (a, b) <- sets, a /= (parent y sets)]

points :: [Edge] -> [String]
points g = pointsRec g []

pointsRec :: [Edge] -> [String] -> [String]
pointsRec [] s = s
pointsRec (e:es) s = pointsRec es (first ++ second ++ s) where
    first
        | not(elem (edgeSrc e) s ) = [edgeSrc e]
        | otherwise = []
    second
        | edgeSrc e == edgeDest e = []
        | not(elem (edgeDest e) s) = [edgeDest e]
        | otherwise = []

-- ********************************************************* TODO *********************************************************
--procuraGrupos :: [Vertex] -> [Edge] -> [[Vertex]]

-- ******************* MAIN *******************
main = do 
    k <- getLine
    c <- getContents
    let 
        l = lines c 
        w = map words l
        vertex = map convert w
        
    print $ drop' ((read k)-1) (reverse $ sort $ kruskal (sort $ createEdges $ vertex))
