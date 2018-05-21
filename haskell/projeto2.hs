-- para usar o sort
import Data.List
import Prelude

-- tipo Vertex tem o nome do vertice e sua lista de coordenadas
data Vertex = Vertex String [Float] deriving Show

-- tipo Edge tem o nome dos dois vertices e a distancia entre eles
data Edge = Edge Float String String deriving (Show, Ord, Eq)

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

-- dado uma lista de pontos (vertices), uma lista de arestas ordenada cresc, retorna a AGM (lista de arestas)
--kruskal :: [Vertex] -> [Edge] -> [Edge]

-- ******************* MAIN *******************
main = do 
    k <- getLine
    c <- getContents
    let 
        l = lines c 
        w = map words l
        vertex = map convert w
        
    print $ drop' ((read k)-1) (reverse $ sort $ createEdges $ vertex)
    --print $ kruskal (sort $ createEdges $ points)
