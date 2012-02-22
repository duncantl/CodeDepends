WriteFunctionNames = c("save", "cat", "write", "write.csv")

setGeneric("makesOutput",
            function(x, ...)
               standardGeneric("makesOutput"))

setMethod("makesPlot", "ScriptNodeInfo",
            function(x, ...)
                any(WriteFunctionNames %in% x@functions))


