library(CodeDepends)


sc = readScript(system.file("samples", "cacherCode", "cacher1.R", package = "CodeDepends"))
info = getInputs(sc)
vars = getVariables(info)
stopifnot(identical(vars, c("tmp", "cities", "classes", "vars", "data", "data", #second one is an update
                            "estimates", "effect", "stderr")))

o = structure(lapply(vars, CodeDepends:::getDependsOn, info, vars), names = vars)

g = makeVariableGraph(system.file("samples", "cacherCode", "cacher1.R", package = "CodeDepends"))
g = makeVariableGraph(system.file("samples", "cacherCode", "cacher2.R", package = "CodeDepends"))



# info[[5]] should have data as an update, not an output
# And should also have names<- as a function.
# So data would be an update variable, implying it is an input
# but it is neither an explicit input or output.


