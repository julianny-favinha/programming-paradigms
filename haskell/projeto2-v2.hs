import Data.List
import Prelude

-- tipo Vertex tem o nome do vertice e sua lista de coordenadas
data Vertex = Vertex String [Float] deriving (Show)

-- tipo Edge tem o nome dos dois vertices e a distancia entre eles
data Edge = Edge Float String String deriving (Show, Ord, Eq)

-- converte uma linha do input em Vertex
convert :: [String] -> Vertex
convert line = do 
    let
      nome = head line
      coords = map read (tail line)
    (Vertex nome coords)

-- cria lista de Edges a partir da distancia entre Vertex
createEdges :: [Vertex] -> [Edge]
createEdges listVertex = [distanceVertex (Vertex p1 coord1) (Vertex p2 coord2) | (Vertex p1 coord1) <- listVertex, (Vertex p2 coord2) <- listVertex, p1 < p2]

-- calcula distancia entre Vertex
distanceVertex :: Vertex -> Vertex -> Edge
distanceVertex (Vertex p1 coord1) (Vertex p2 coord2) = Edge (sqrt $ sum $ zipWith (\ f1 f2 -> (f1 - f2)**2) coord1 coord2) p1 p2

--exemplo: ["a", "b", "c"] retorna [["a"], ["b"], ["c"]]
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

--exemplo: encontraLista [["1","2"],["3","4"]] "5" retorna []
--exemplo: encontraLista [["1","2"],["3","4"]] "4" retorna ["3","4"]
encontraLista :: [[String]] -> String -> [String]
encontraLista [] _ = []
encontraLista (c : cs) v
    | elem v c = c
    | otherwise = encontraLista cs v

--exemplo: mergeListas "1" "3" [["1","2"],["3","4"], ["5"]] retorna [["1","2","3","4"], ["5"]]
mergeListas :: String -> String -> [[String]] -> [[String]]
mergeListas v1 v2 c = filter (/= (encontraLista c v2)) (filter (/= (encontraLista c v1)) ([(encontraLista c v1) ++ (encontraLista c v2)] ++ c))

{-
exemplo: [["a","b"], ["c","d"], ["e"]] retorna o print

["a","b"]
["c","d"]
["e"]
-}
imprimeLista [] = return ()
imprimeLista (x : xs) =
  do
    print x
    imprimeLista xs

kruskal :: [Edge] -> [Edge]
kruskal arestas = kruskalAux (componentes $ vertices arestas) [] arestas

kruskalAux :: [[String]] -> [Edge] -> [Edge] -> [Edge]
kruskalAux _ agm [] = agm
kruskalAux componentes agm (aresta@(Edge d v1 v2): es)
    | not (elem v2 (encontraLista componentes v1)) = kruskalAux (mergeListas v1 v2 componentes) (aresta: agm) es
    | otherwise = kruskalAux componentes agm es

--dado um vertice e uma lista de arestas, encontra os vertices adjacentes desse vertice
adjacents :: String -> [Edge] -> [String] -> [String]
adjacents _ [] adj = adj
adjacents v ((Edge _ v1 v2) : es) adj = adjacents v es (u ++ adj) where
    u
        | v == v1 = [v2]
        | v == v2 = [v1]
        | otherwise = []

--dfs :: recebe lista de nomes dos vertices, agm (com as k-1 arestas a menos) e recebe um acumulador (components)
--dfs :: retorna lista de componentes conexas
dfs :: [String] -> [Edge] -> [[String]] -> [[String]]
dfs [] _ components = components
dfs (v : vs) edges components
    -- se v nao é elemento em components, vai chamar dfs-visit e o resultado vai ser appendado em components
    | (encontraLista components v) == [] = dfs vs edges (components ++ [dfsVisit edges [] v])
    | otherwise = dfs vs edges components

--dfsVisit :: dado o vertice e a agm, retorna o componente
dfsVisit :: [Edge] -> [String] -> String -> [String]
dfsVisit edges acc v
    | (adjacents v edges []) == [] = v : acc
    | otherwise = foldl (++) [] (map (dfsVisit (removeEdges v edges) (acc ++ [v])) (adjacents v edges []))

removeEdges :: String -> [Edge] -> [Edge]
removeEdges v edges = filter (\(Edge _ v1 v2) -> ((v2 /= v) && (v1 /= v))) edges 

main = do 
    k <- getLine
    c <- getContents
    let 
        l = lines c 
        w = map words l
        vertex = map convert w
        agm = drop ((read k)-1) (reverse (sort (kruskal (sort (createEdges vertex)))))
        
    print $ dfs (vertices (createEdges vertex)) agm [] 