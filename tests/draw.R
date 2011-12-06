library(CodeDepends)
library(Rgraphviz)
sc = readScript(system.file("samples", "parallel.R", package = "CodeDepends"))

g = makeVariableGraph(,sc)
plot(g)

g = makeTaskGraph(,sc)
plot(g)
