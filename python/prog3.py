import numpy as np
from scipy.sparse import csr_matrix
from scipy.sparse.csgraph import dijkstra

# verifica se há arestas multiplas no dicionario
def countEdge(dic, key):
    count = 0

    for i in dic:
        si, ti, modei = i
        keyi = (si, ti)
        if keyi == key:
            count += 1

    return count

# encontra key de um certo valor
def encontraKey(dic, v):
    for key, value in dic.items():
        if value == v:
            return key, value

# retorna lista de predecessores invertida(i.e. path)
def pred(predecessors, p, init):
    if (p == init):
        return []
    return pred(predecessors, predecessors[p], init) + [predecessors[p]]

# retorna mode, destino e tempo/custo de uma aresta
def getMode(graph, edge, ind, keys):
    for key, time in graph.items():
        i, j, mode = key

        if "(new node)" in i:
            ki, kj = keys[i], keys[j]
            if((ki,kj) == edge):
                i, j, newmode = mode
                return newmode, j, time

        if "(new node)" in j:
            ki, kj = keys[i], keys[j]
            if((ki,kj) == edge):
                i, j, newmode = mode
                return 'ignore', 'ignore', 0.0

        ki, kj = keys[i], keys[j]

        if ((ki,kj) == edge):
            return mode, j, time

def sumTimes(edges, begin, end):
    sum = 0
    for index in range(begin, end):
	    sum += edges[index][3]

    return sum

def edgeExists(edges, e1a, e2a):
    for edge in edges:
        e1b, e2b, mode, time = edge
        if e1a == e1b and e2a == e2b:
            return True

    return False

# função main
if __name__ == "__main__":
    graph = {}
    subGraph ={}
    newGraph = {}
    waitTimes = {}
    vehicles = {}

    # Lê linhas do grafo
    while True:
        line = input()
        if line == '':
            break
        else:
            i, j, mode, time = line.split(" ")
            graph[(i,j, mode)] = float(time)
            if mode in vehicles.keys():
            	vehicles[mode].append((i, j, time))
            else:
            	vehicles[mode] = [(i, j, time)]

    subGraph = graph
    # Cria novas arestas dos grafo
    for edges in vehicles.values():
        value.sort(key=lambda tup: tup[0])

    for edges in vehicles.values():
        for i in range(0, len(edges)-1):
            for j in range(i+1, len(edges)):
                # nova aresta
                if edges[i][0] != edges[j][1] and not edgeExists(subGraph, edges[i][0], edges[j][1]):
                    subGraph[(edges[i][0], edges[j][1], edges[i][2])] = sumTimes(edges, i, j+1)

    print("SUBGRAPH")
    print(subGraph)

    # Lê linhas de tempo de espera
    while True:
        line = input()
        if line == '':
            break
        else:
            mode, time = line.split(" ")
            waitTimes[mode] = float(time)

    # Lê linha de início-fim
    init, end = input().split(" ")

    # adiciona tempos de espera ao grafo
    for key, time in graph.items():
        i, j, mode = key

        if mode != 'a-pe':
            time += waitTimes[mode]

        graph[key] = time

    # Verifica se há arestas múltiplas
    for key, time in graph.items():
        s, t, mode = key
        oc = countEdge(graph, (s,t))

        if (oc > 1):
            newGraph[(s, s + ' ' + t + ' ' + mode + "(new node)", key)] = 0.1 # não pode ser zero
            newGraph[(s + ' ' + t + ' ' + mode + "(new node)", t, key)] = time
        else:
            newGraph[key] = time

    # controi dicionario de índices
    i = 1
    dicIndex = {}
    dicIndex[init] = 0
    for key, time in newGraph.items():
        s, t, mode = key

        if s not in dicIndex:
            dicIndex[s] = i
            i += 1

        if t not in dicIndex:
            dicIndex[t] = i
            i += 1

    # controi lista de chaves
    n = len(dicIndex)
    keys = n*[0]
    for key, value in dicIndex.items():
        keys[value] = key

    # constroi sparse matrix
    sparse = np.zeros((n, n))
    for key, time in newGraph.items():
        s, t, mode = key
        sparse[dicIndex[s]][dicIndex[t]] = time
    sparse = csr_matrix(sparse)

    # aplica djikstra à sparse matrix
    distances, predecessors = dijkstra(sparse, return_predecessors=True, directed=True)

    # obtem path do início ao fim
    path = pred(predecessors[0], dicIndex[end], dicIndex[init]) + [dicIndex[end]]

    # imprime path
    total = 0
    pathAux = init + ' '
    edgesAux = zip(path[:-1], path[1:])
    for edge in edgesAux:
        mode, t, time = getMode(newGraph, edge, keys, dicIndex)

        if mode != 'ignore':
            pathAux += mode + ' ' + t + ' '

        total += time

    print(pathAux)
    print(total)
