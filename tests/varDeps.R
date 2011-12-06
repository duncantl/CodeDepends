library(CodeDepends)

 # With cacher1.R, letters appears, but doesn't show up in the graph.
 # sc = readScript("cacher1.R")

sc = readScript(system.file("samples", "cacherCode", "cacher.R", package = "CodeDepends"))
info = getInputs(sc)
vars = getVariables(info)

o = structure(lapply(vars, CodeDepends:::getDependsOn, info, vars), names = vars)

g = makeVariableGraph(system.file("samples", "cacherCode", "cacher2.R", package = "CodeDepends"))


# info[[5]] should have data as an update, not an output
# And should also have names<- as a function.
# So data would be an update variable, implying it is an input
# but it is neither an explicit input or output.


