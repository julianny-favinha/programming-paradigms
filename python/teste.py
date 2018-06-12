graph = {("a", "b", "linha330"): 5, ("b", "c", "linha330"): 14, ("c", "d", "linha330"): 6}
        	
def transformGraphList(graph):
	edges = []
	for key in graph.keys():
		i, j, mode = key
		edges.append((i, j, mode, graph[key]))

	return edges

def sumTimes(edges, begin, end):
    sum = 0
    for index in range(begin, end):
	    sum += edges[index][3]

    return sum

def sumEdges(graph, v1, v2, begin, end):
	edges = transformGraphList(graph)
	edges.sort(key=lambda tup: tup[0])
	return sumTimes(edges, begin, end)


print(sumEdges(graph, "a", "d", 1, 2+1))
