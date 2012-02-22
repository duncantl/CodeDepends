setClass("ScriptNode", representation(code = "{", taskType = "character", id = "character"),
                         prototype = list(id = as.character(NA)))

setClass("Script", representation(location = "character"), contains = "list")
setClass("AnnotatedScript", contains = "Script")

setClass("ScriptInfo", contains = "list")
setClass("ScriptNodeInfo",
          representation(files = 'character',
                         strings = 'character',
                         libraries = 'character',
                         inputs = 'character',
                         outputs = 'character',
                         updates = 'character',
                         functions = 'character',
                         removes = 'character'))

setOldClass(c("DetailedVariableTimeline", "data.frame"))

setAs("ScriptNode", "ScriptNodeInfo",
      function(from)
        getInputs(from))

setAs("Script", "ScriptInfo",
      function(from)
        getInputs(from))

setAs("expression", "ScriptNodeInfo",
      function(from)
        getInputs(from))
setAs("language", "ScriptNodeInfo",
      function(from)
        getInputs(from))

setMethod("[", signature(x = "Script", i = "vector", j = "missing"),
          function(x, i, j, ..., drop = TRUE) {
                  x@.Data <- x@.Data[i]
                  x
          })
