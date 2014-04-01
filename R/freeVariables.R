#
# Given a set of expressions/script, find which variables
# are defined and never used and which are never defined.
#
freeVariables = undefinedVariables =
function(sc, info = as(sc, "ScriptInfo"))
{
   pos = seq(from = 1, to = length(info))
   outputs = lapply(info[ - length(pos) ], function(x) x@outputs)
   ans = character()
   defs = character()
   for(i in pos) {
      el = info[[i]]
      ans = c(ans, setdiff(el@inputs, defs))
      defs = c(defs, el@outputs)
   }
   ans
}


unusedVariables =
function(sc, info = as(sc, "ScriptInfo"))
{
   pos = seq(from = 1, to = length(info) - 1L)
   inputs = lapply(info[pos + 1], function(x) x@inputs)
   ans = character()
   for(i in pos) {
      el = info[[i]]
      ans = c(ans, setdiff(el@outputs, unlist(inputs)[i:length(pos)]))
   }
   ans
}
