-- para usar o sort
import Data.List
import Prelude

-- tipo Vertex tem o nome do vertice e sua lista de coordenadas
data Vertex = Vertex String [Float] deriving (Show)

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

-- 
componentes :: [String] -> [[String]]
componentes vertices = map (\x -> [x]) vertices

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

kruskal :: [Edge] -> [Edge]
kruskal arestas = kruskalAux (componentes $ vertices arestas) [] arestas

kruskalAux :: [[String]] -> [Edge] -> [Edge] -> [Edge]
kruskalAux _ agm [] = agm
kruskalAux componentes agm (aresta@(Edge d v1 v2): es)
    | not (elem v2 (encontraLista componentes v1)) = kruskalAux (mergeListas v1 v2 componentes) (aresta: agm) es
    | otherwise = kruskalAux componentes agm es

encontraLista :: [[String]] -> String -> [String]
encontraLista [] _ = []
encontraLista (c : cs) v
    | elem v c = c
    | otherwise = encontraLista cs v

mergeListas :: String -> String -> [[String]] -> [[String]]
mergeListas v1 v2 c = filter (/= (encontraLista c v2)) (filter (/= (encontraLista c v1)) ([(encontraLista c v1) ++ (encontraLista c v2)] ++ c))

main = do 
    k <- getLine
    c <- getContents
    let 
        l = lines c 
        w = map words l
        vertex = map convert w
        
    --print $ drop' ((read k)-1) (reverse $ sort $ kruskal (sort $ createEdges $ vertex))
    print $ kruskal (sort $ createEdges $ vertex)