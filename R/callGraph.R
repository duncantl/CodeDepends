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


setMethod("makeCallGraph", "function",
          function(obj, all = FALSE, recursive = TRUE, ...) {
              name = as.character(substitute(obj))
              makeCallGraph(list(obj), all, recursive, funNames = name, ...)
          })

# Do the call graph for methods, i.e. which methods call which other methods.
# Can we tell this.
# Deal with global variables, i.e. non function references.


setMethod("makeCallGraph", "character",
          function(obj, all = FALSE, recursive = TRUE, ...) {
    
    path = search()
    ispkg = grepl("^package:", obj)
    ## we have a vector (length >=1) of package with 
    ## the 'package:' prefix
    if(all(ispkg)) {
        ## this will work even if obj is length 1 and I don't
        ## want to put the logic in 2 places...
        funs = lapply(obj, getFunctions)
        
        return(makeCallGraph(unlist(funs, recursive = FALSE), all = all, recursive = recursive,
                             names = unlist(lapply(funs, names)),
                             packages = rep(gsub("^package:", "", obj), sapply(funs, length))))
    } else if(any(ispkg)) { # mix of packages and functions. weird but why not
        funlst = as.list(obj)
        funlst[ispkg] = lapply(obj[ispkg], getFunctions)
        ## the following will error if there is no function found
        ## of that name, so no need to further check exists.
        funlst[!ispkg] = lapply(obj[!ispkg],function(x) structure(get(x, mode= "function"), names = x))
        ## ugh this name and package vector munging stuff is painful :-/
        pkgnames = as.list(rep("", times = length(obj)))
        
        pkgnames[ispkg] = mapply(function(nm, funs) rep(nm, times = length(funs)),
                                 nm = gsub("^packages:", "", obj[ispkg]),
                                 funs = funlst[ispkg], SIMPLIFY=FALSE)
        pkgnames = unlist(pkgnames)
        names(funlst)[!ispkg] = obj[!ispkg]
        funlst = unlist(funlst, recursive = FALSE)
        return(makeCallGraph(funlst, all = all, recursive = recursive, names = names(funlst),
                             packages = pkgnames))
    } else if (length(obj) > 1) {
        funs = lapply(obj, get, mode="function")
        names(funs) = obj
        return(makeCallGraph(funs, all = all, recursive = recursive,
                             names = obj, packages = rep("", times = length(obj))))
    }
    
    if(exists(obj, mode = "function"))
        return(makeCallGraph(structure(list(get(obj, mode = "function")), names = obj), all = all, recursive = recursive,
                             packages = "", ...))
    

    ## if( !is.na(w <- match(obj, path)) || !is.na(w <- match(obj, gsub("^package:", "", path)))) {
    ##     obj = getFunctions(w)
    ##     return(makeCallGraph(obj, recursive = recursive, ...))
    ## }

    stop("Don't know how to make a call graph from this string: ", obj)
})

setMethod("makeCallGraph", "list",
          function(obj, all = FALSE, recursive = TRUE, funNames = names(obj), packages = attr(obj, "packages"), ...) {
               # Assume all functions.
            require(graph)
            ids = funNames
            edges = lapply(obj, function(f) {
                                  calls = findGlobals(f, merge = FALSE)$functions
                                  if(all)
                                    return(calls)
                                  list(edges = match(calls[calls %in% ids], ids))
                              })


            if(all) {
              z = lapply(edges, function(x) x[ !( x %in%  c("|", "||")) ])
              ids = unique(c(unlist(z), ids))
              edges = replicate(length(ids), character(), FALSE)
              names(edges) = ids
              edges[ names (z) ] = z
#              edges = unlist(lapply(edges, function(x) {
#                                             i = match(x, ids)
#                                             list(edges = setdiff(ids[i], x))
#                                           }))
            }
            
            g = new("CallGraph", nodes = ids, edgeL = edges, edgemode = "directed")

            if(length(packages)) {
                nodeDataDefaults(g, "package") <- ""
                nodeData(g, names(obj), "package") <- packages
            }
            
            g
          })


setClass("CallGraph", contains = "graphNEL")

setMethod("plot", "CallGraph",
          function (x, y, ..., name = "", subGList = list(), 
                    attrs = list(), nodeAttrs = list(), edgeAttrs = list(), 
                    recipEdges = c("combined", "distinct"), colors= c("red", "blue"))
          {
              ids = nodes(x)
              pkg = unlist(nodeData(x, , "package"))
              nodeAttrs$color = structure(colors[factor(pkg)], names = ids)

              callNextMethod(x, y, ..., name = name, subGList = subGList, attrs = attrs, nodeAttrs = nodeAttrs, edgeAttrs = edgeAttrs, recipEdges = recipEdges)
          })

