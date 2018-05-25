-- exemplo foi de Kruskal foi encontrado em http://echochamber.me/viewtopic.php?t=46361

-- para usar o sort
import Data.List
import Prelude

-- tipo Vertex tem o nome do vertice e sua lista de coordenadas
data Vertex = Vertex String [Float] deriving Show

-- tipo Edge tem o nome dos dois vertices e a distancia entre eles
data Edge = Edge Float String String deriving (Show, Ord, Eq)

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
kruskal g = kruskalRec g [] [(x, x) | x <- (vertices g)]

kruskalRec :: [Edge] -> [Edge] -> [(String, String)] -> [Edge]
kruskalRec [] progress sets = progress
kruskalRec ((Edge d v1 v2) : es) progress sets
    | joined v1 v2 sets = kruskalRec es progress sets
    | otherwise = kruskalRec es ((Edge d v1 v2) : progress) (join v1 v2 sets)

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

parent :: String -> [(String, String)] -> String
parent elem sets
    | fst match == snd match = fst match
    | otherwise = parent (snd match) sets
    where
        match = head [(x, y) | (x, y) <- sets, x == elem]

-- retorna a lista de vertices
vertices :: [Edge] -> [String]
vertices g = verticesRec g []

verticesRec :: [Edge] -> [String] -> [String]
verticesRec [] str = str
verticesRec ((Edge _ v1 v2) : es) str = verticesRec es (first ++ second ++ str) where
    first
        | not(elem v1 str) = [v1]
        | otherwise = []
    second
        | not(elem v2 str) = [v2]
        | otherwise = []

-- ******************************************************* TODO *******************************************************
--procuraGrupos :: [Vertex] -> [Edge] -> [[Vertex]]

-- ******************* MAIN *******************
main = do 
    k <- getLine
    c <- getContents
    let 
        l = lines c 
        w = map words l
        vertex = map convert w
        
    --print $ drop' ((read k)-1) (reverse $ sort $ kruskal (sort $ createEdges $ vertex))
    print $ kruskal (sort $ createEdges $ vertex)