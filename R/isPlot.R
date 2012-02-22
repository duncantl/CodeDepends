PlotFunctionNames = c("plot", "hist", "points", "lines", "legend", "jitter", "segments")

setGeneric("makesPlot", function(x, ...)
                           standardGeneric("makesPlot"))

setMethod("makesPlot", "ScriptNodeInfo",
            function(x, ...)
                any(PlotFunctionNames %in% x@functions))

