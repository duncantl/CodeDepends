library(graph)
library(Rgraphviz)

nodes = paste("Task", 1:8, sep = "")
edges = vector("list", length(nodes))
names(edges) = nodes
edges[[1]] = edges[[2]] = edges[[3]] = list(edges = 4)

edges[[4]] = list(edges = 5)

edges[[5]] = list(edges = c(6, 7))
edges[[7]] = list(edges = 8)

g = new("graphNEL", nodes = nodes, edgeL = edges, edgemode = "directed")
