getToplevelVariables =
function(code, ...)
{
    info = as(code, "ScriptInfo")
    #?? can we just call CodeDepends::getVariables()
    # Also, move this to CodeDepends.
    unique(unlist(c(lapply(info, slot, "outputs"), lapply(info, slot, "updates"))))
}


setGeneric("getPackages",
           function(code, ...)
           standardGeneric("getPackages"))

setMethod("getPackages",
          "character",
          function(code, ...) {

              if(file.exists(code))
                  sc = readScript(code)
              else
                  sc = parse(text = code)
              
              getPackages(sc, ...)
          })

setMethod("getPackages",
          "expression",
          function(code, ...) {
              i = getInputs(code)
              getPackages(i, ...)
          })

setMethod("getPackages",
          "Script",
          function(code, ...) 
 {
     getPackages(as(code, "ScriptInfo"), ...)
 })

setMethod("getPackages",
          "ScriptInfo",
          function(code, counts = FALSE, ...) 
          {
              ans = unlist(sapply(code, slot, "libraries"))
              if(counts)
                  table(ans)
              else
                  unique(ans)
          })


setMethod("getPackages",
          "ScriptNodeInfo",
          function(code, counts = FALSE, ...)  {
              ans = code@libraries
              if(counts)
                  table(ans)
              else
                  unique(ans)             
          })
             


################

# Need methods as we do different things with different inputs,
# and not everything should be coerced to a ScriptNodeInfo as below.

setGeneric("getOutputVars",
           function(x, ...)
              standardGeneric("getOutputVars"))

tmp = function(x, merge = TRUE)
{
    i = as(x, "ScriptNodeInfo")
    ans = i@outputs
    if(merge)
        ans = c(ans, i@updates)
    ans
}

setMethod("getOutputVars", "ScriptNodeInfo", tmp)
setMethod("getOutputVars", "call", tmp)
setMethod("getOutputVars", "<-", tmp)
setOldClass("=")
setMethod("getOutputVars", "=", tmp)

setMethod("getOutputVars", "character",
          function(x, merge = TRUE) {
              if(file.exists(x))
                  e = parse(x)
              else
                  e = parse(text = x)
              getOutputVars(e, merge)
          })

setMethod("getOutputVars", "expression",
          function(x, merge = TRUE) {
              if(length(x) == 1)
                  getOutputVars(as(x[[1]], "ScriptNodeInfo"), merge)
              else
                  getOutputVars(as(x, "ScriptInfo"), merge)                  
          })

setMethod("getOutputVars", "ScriptInfo",
          function(x, merge = TRUE) {
              unique(unlist(lapply(x, getOutputVars, merge)))
          })

setGeneric("getInputVars",
           function(x, ...)
            standardGeneric("getInputVars"))

getInputVars =
function(x, merge = TRUE, functions = FALSE)
{
    i = as(x, "ScriptNodeInfo")
    ans = i@inputs
    if(merge)
        ans = c(ans, i@updates)
    if(functions)
        ans = c(ans, names(i@functions))
    ans
}





