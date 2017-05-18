setClass("ScriptNode", representation(code = "{", taskType = "character", id = "character"),
                         prototype = list(id = as.character(NA)))



setClass("Script", representation(location = "character"), contains = "list")
## setMethod("names", "VirtList", function(x) x@.names)
## setMethod("names<-", "VirtList", function(x, value) {
##     x@.names <- value
##     x
## })

## setMethod("[", "VirtList",
##            function(x, i, j, ..., drop = TRUE) {
##     if(is(i, "character")) {
##         i = match(i, names(x))
##         i = i[!is.na(i)]
##     }
##     dat = x@.Data[i, drop = drop]
##     new(class(x), dat, .names = names(x)[i])
## })

## setMethod("[[", "VirtList",
##            function(x, i, j, ..., drop = TRUE) {
##     if(is(i, "character")) {
##         i = match(i, names(x))
##         i = i[[!is.na(i)]]
##     }
##     x@.Data[[i, drop = drop]]
    
## })


## setMethod("[<-", "VirtList",
##            function(x, i, j, ..., drop = TRUE, value) {

##     base::`[<-`(x, i, drop = drop, value = value)
##     if(length(names(x)) > length(x))
##         names(x) = head(names(x), length(x))
##     else if (length(names(x)) < length(x))
##         names(x) = c(names(x), rep("", times = length(x) - length(names(x))))
##     x
    
## })

## setMethod("[[<-", "VirtList",
##            function(x, i, j, ..., drop = TRUE, value) {

##     base::`[[<-`(x, i, drop = drop, value = value)
##     if(length(names(x)) > length(x))
##         names(x) = head(names(x), length(x))
##     else if (length(names(x)) < length(x))
##         names(x) = c(names(x), rep("", times = length(x) - length(names(x))))
##     x
    
## })

setClass("AnnotatedScript", contains = "Script")



setClass("ScriptInfo", contains = "list")
setClass("ScriptNodeInfo",
          representation(files = 'character',
                         strings = 'character',
                         libraries = 'character',
                         inputs = 'character',
                         outputs = 'character',
                         updates = 'character',
                         functions = 'logical', # indicating locally defined or not or NA, and the functions names are the names for the vector. #  'character',
                         removes = 'character',
                         nsevalVars = 'character',
                         sideEffects = 'character',
                         code = "ANY"))

setOldClass(c("DetailedVariableTimeline", "data.frame"))

setAs("ScriptNode", "ScriptNodeInfo",
      function(from)
        getInputs(from))

scriptInfo = function(sc) as(sc, "ScriptInfo")

setAs("Script", "ScriptInfo",
      function(from)
        getInputs(from))

setAs("expression", "ScriptNodeInfo",
      function(from)
        getInputs(from))
setAs("language", "ScriptNodeInfo",
      function(from)
        getInputs(from))

setMethod("[", signature(x = "Script", i = "character", j = "missing"),
          function(x, i, j, ..., drop = TRUE) {
    inds = match(i, names(x))
    inds = inds[!is.na(inds)]
    x[inds, drop = drop]
    })
setMethod("[", signature(x = "Script", i = "vector", j = "missing"),
          function(x, i, j, ..., drop = TRUE) {
                  x@.Data <- x@.Data[i]
                  x
          })

setMethod("$", "Script",
          function(x, name)
            invisible( sourceVariable(name, x)) )
