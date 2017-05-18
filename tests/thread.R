library(CodeDepends)
 # Simple but with a code block that is stand-alone/unrelated.
p <- readScript(system.file("samples", "dual.R", package = "CodeDepends"))
## need formulaInputs=TRUE because there is an lm(y~x) call in there ...

ethread = getExpressionThread("fit", p, formulaInputs = TRUE) 
stopifnot(length(ethread) == 3)

dthread = getDependsThread("fit", getInputs(p, formulaInputs=TRUE))
stopifnot(identical(dthread, c(1L, 2L, 4L)))
# With redefintions of variables
s <- readScript(system.file("samples", "sitepairs.R", package = "CodeDepends"))
getExpressionThread("covs", s)
getDependsThread("covs", as(s, "ScriptInfo"))

# With redefintions of variables
p <- readScript(system.file("samples", "results-multi.R", package = "CodeDepends"))
getExpressionThread("results", p)
getDependsThread("results", as(p, "ScriptInfo"))



