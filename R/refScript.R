
setRefClass("DynScript",
             fields = list(location = "character",
                           timestamp = "POSIXct",
                           codeFrags = "Script",
                           info = "ScriptInfo",
                           verbose = "logical"),
             methods = list(
               initialize = function(doc, ...) {
                  location <<- doc
                  verbose <<- FALSE
                  timestamp <<- as.POSIXct(NA)
                  update()
                  callSuper(...)
                  .self
                },
               source = function(var, ...) {
                 update()
                 sourceVariable(var, frags = codeFrags, ...)
               },
               update = function() {
                 tm = file.info(location)[1, "mtime"]
                 if(is.na(timestamp) || tm > timestamp) {
                   if(verbose)
                       cat("re-reading", location, "\n")
                   timestamp <<- Sys.time()
                   codeFrags <<- readScript(location)
                 }
               },
               get = function(var, ...) {
                 .self$source(var, ...)
                 base::get(var)
               }
              ))

#XXX recompute one  expression to update/redefine a variable

DynScriptFactory <- getRefClass("DynScript")
#DynScriptFactory$accessors(names(DynScriptFactory$fields()))

updatingScript =
function(doc, ...)
{
  DynScriptFactory$new(doc, ...)
}

setAs("DynScript", "Script",
      function(from) {
         from$update()
         from$codeFrags
       })

setMethod("getInputs", "DynScript",
           function(e, collector = inputCollector(), basedir = ".", reset = FALSE, ...) {
             getInputs(as(e, "Script"), collector, basedir, reset, ...)
           })
