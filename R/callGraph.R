setGeneric("makeCallGraph", 
function(obj, all = FALSE, ...)
 standardGeneric("makeCallGraph"))

getFunctions =
function(pos, syms = objects(pos))
{
   objs = lapply(syms, function(x) { f = get(x, pos); if(is.function(f)) f else NULL})
   names(objs) = syms
   objs[!sapply(objs, is.null)]
}

# Do the call graph for methods, i.e. which methods call which other methods.
# Can we tell this.
# Deal with global variables, i.e. non function references.

setMethod("makeCallGraph", "character",
          function(obj, all = FALSE, recursive = TRUE, ...) {

            if(length(obj) > 1) {
              return(makeCallGraph(getFunctions(, obj), all = all))
            }
            
            if(exists(obj, mode = "function"))
              return(makeCallGraph(get(obj, mode = "function"), recursive = recursive, ...))

            path = search()
            if( !is.na(w <- match(obj, path)) || !is.na(w <- match(obj, gsub("^package:", "", path)))) {
               obj = getFunctions(w)
               return(makeCallGraph(obj, recursive = recursive, ...))
            }

            stop("Don't know how to make a call graph from this string: ", obj)
          })

setMethod("makeCallGraph", "list",
          function(obj, all = FALSE, recursive = TRUE, ...) {
               # Assume all functions.
            require(graph)
            ids = names(obj)
            edges = lapply(obj, function(f) {
                                  calls = findGlobals(f, merge = FALSE)$functions
                                  if(all)
                                    return(calls)
                                  list(edges = match(calls[calls %in% ids], ids))
                                })

            if(all) {
              ids = unlist(edges)
              edges = lapply(edges, function(x) {
                                          list(edges = match(x, ids))
                                    })
            }
            new("graphNEL", nodes = ids, edgeL = edges, edgemode = "directed")
          })


