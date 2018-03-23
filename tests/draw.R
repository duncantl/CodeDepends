library(CodeDepends)
library(Rgraphviz)
sc = readScript(system.file("samples", "parallel.R", package = "CodeDepends"))

g = makeVariableGraph(,sc)
plot(g)

g = makeTaskGraph(,sc)
plot(g)



g2 = makeCallGraph("package:CodeDepends") # package with prefix
g3 = makeCallGraph("getInputs") # single fun
g4 = makeCallGraph(c("package:CodeDepends", "package:Rgraphviz")) #multiple pkgs
g5 = makeCallGraph(c("package:CodeDepends", "edgeData")) #mix pkg and fun
