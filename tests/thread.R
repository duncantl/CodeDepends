library(CodeDepends)
 # Simple but with a code block that is stand-alone/unrelated.
p <- readScript(system.file("samples", "dual.R", package = "CodeDepends"))
getExpressionThread("fit", p)
getDependsThread("fit", as(p, "ScriptInfo"))

# With redefintions of variables
s <- readScript(system.file("samples", "sitepairs.R", package = "CodeDepends"))
getExpressionThread("covs", s)
getDependsThread("covs", as(s, "ScriptInfo"))

# With redefintions of variables
p <- readScript(system.file("samples", "results-multi.R", package = "CodeDepends"))
getExpressionThread("results", p)
getDependsThread("results", as(p, "ScriptInfo"))



