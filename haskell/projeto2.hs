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

-- encapsula cada elemento de uma lista dentro de uma lista
-- exemplo: ["a", "b", "c"] retorna [["a"], ["b"], ["c"]]
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

-- encontra a lista que possui o elemento v
--exemplo: findList [["1","2"],["3","4"]] "5" retorna []
--exemplo: findList [["1","2"],["3","4"]] "4" retorna ["3","4"]
findList :: [[String]] -> String -> [String]
findList [] _ = []
findList (c : cs) v
    | elem v c = c
    | otherwise = findList cs v

-- dado um vertice e uma lista de arestas, encontra os vertices adjacentes desse vertice
adjacents :: String -> [Edge] -> [String] -> [String]
adjacents _ [] adj = adj
adjacents v ((Edge _ v1 v2) : es) adj = adjacents v es (u ++ adj) where
    u
        | v == v1 = [v2]
        | v == v2 = [v1]
        | otherwise = []

-- remove arestas onde possui v como um dos vertices
removeEdges :: String -> [Edge] -> [Edge]
removeEdges v edges = filter (\(Edge _ v1 v2) -> ((v2 /= v) && (v1 /= v))) edges 

--exemplo: mergeLists "1" "3" [["1","2"],["3","4"], ["5"]] retorna [["1","2","3","4"], ["5"]]
mergeLists :: String -> String -> [[String]] -> [[String]]
mergeLists v1 v2 c = filter (/= (findList c v2)) (filter (/= (findList c v1)) ([(findList c v1) ++ (findList c v2)] ++ c))

sortComponents :: [[String]] -> [[String]]
sortComponents components = map sort components

-- imprime lista de componentes de uma forma diferente da convencional
{-
exemplo: [["a","b"], ["c"]] imprime

["a","b"]
["c"]
-}
printList [] = return ()
printList (x : xs) =
  do
    print x
    printList xs

{-
Algoritmo de Kruskal
-}
kruskal :: [Edge] -> [Edge]
kruskal arestas = kruskalAux (componentes $ vertices arestas) [] arestas

kruskalAux :: [[String]] -> [Edge] -> [Edge] -> [Edge]
kruskalAux _ agm [] = agm
kruskalAux componentes agm (aresta@(Edge d v1 v2): es)
    | not (elem v2 (findList componentes v1)) = kruskalAux (mergeLists v1 v2 componentes) (aresta: agm) es
    | otherwise = kruskalAux componentes agm es

{-
Algoritmo de Busca em profundidade
-}
-- recebe lista de nomes dos vertices, agm (com as k-1 arestas a menos) e recebe um acumulador (components)
-- retorna lista de componentes conexas
dfs :: [String] -> [Edge] -> [[String]] -> [[String]]
dfs [] _ components = components
dfs (v : vs) edges components
    -- se v nao é elemento em components, vai chamar dfs-visit e o resultado vai ser appendado em components
    | (findList components v) == [] = dfs vs edges (components ++ [nub (dfsVisit edges [] v)])
    | otherwise = dfs vs edges components

-- dado o vertice e a agm, retorna o componente
dfsVisit :: [Edge] -> [String] -> String -> [String]
dfsVisit edges acc v
    | (adjacents v edges []) == [] = v : acc
    | otherwise = foldl (++) [] (map (dfsVisit (removeEdges v edges) (acc ++ [v])) (adjacents v edges []))

{- 
MAIN
-}
main = do 
    k <- getLine
    c <- getContents
    let 
        l = lines c 
        w = map words l
        vertex = map convert w
        agm = drop ((read k)-1) (reverse (sort (kruskal (sort (createEdges vertex)))))
        
    printList $ sortComponents ((dfs (vertices (createEdges vertex)) agm []))