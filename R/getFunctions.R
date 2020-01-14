######

getFunctionGlobals =
    #
    # Get the list of functions and the globals they use from an entire script
    # So this finds functions defined at the top-level in the script and also
    # in source() files.
    #
    # getFunctionGlobals("../tests/getFunctionDefsEg.R")
    #
    #
function(sc, funs = getFunctionDefs(sc, envir), envir = globalenv())
{
    ans = lapply(funs, function(x) getGlobals(x)$variables)
    ans[sapply(ans, length) > 0]
}

###############################################


getFunctionDefs =
function(sc, envir = globalenv(), source = getSourceFunctions(sc, envir, byFile = FALSE))
{
    if(is.character(sc))
        sc = readScript(sc)
    
    isFun = sapply(sc, function(x) is.call(x) && class(x) %in% c("=", "<-") && is.name(x[[2]])
                                      && is.call(x[[3]]) && is.name(x[[3]][[1]]) && x[[3]][[1]] == "function")

    funs = list()    
    if(any(isFun))
       funs = structure(lapply(sc[isFun], function(e) eval(e[[3]], envir)),
                        names = sapply(sc[isFun], function(e) as.character(e[[2]])))

    if(length(source))
        c(funs, source)
    else
        funs
}


getSourceFunctions =
    #
    # Takes the result of readScript() (or parse() or the name of a file containing R code)
    # and finds the top-level commands that are source(filename) where file name is a literal
    # value. It then finds and evaluates the top-level functions in these files to get a list
    # of the functions. It does not assign the functions in the global environment or the one provided by envir,
    # but just returns the objects, along with the associated would-be names of the functions in the list.
    # This doesn't yet handle cases where
    #  1. the file being sourced is provided as a variable which we could use getVariableDepends() to find the literal value.
    #  2. source() is done in a loop - for()/lapply().
    #
    # It does handle relative directories, i.e. calling this function 
    #
    #
    #  sc2 = readScript("../tests/getFunctionDefsEg.R")
    #  getSourceFunctions(sc2)
    # unlist the result, keeping the function names, but discarding which file it came from.
    #  getSourceFunctions(sc2, byFile = FALSE)
    #
    # shows can specify script by file name.
    #  getSourceFunctions("../tests/getFunctionDefsEg.R")
    #
function(sc, envir = globalenv(), byFile = TRUE, dir = dirname(sc@location), processed = character())
{
    if(is.character(sc))
        sc = readScript(sc)
    
    ff = sapply(sc, function(e)
                      if(is.call(e) && is.name(e[[1]]) && e[[1]] == "source") {
                        f = match.call(source, e)$file
                        if(is.character(f))
                            f
                        else {
                            warning("skipping source() with non-literal value:", paste(deparse(f), collapse = " "))
                            NA
                        }
                    } else NA)
    ff = ff[!is.na(ff)]

       # Need to track the setwd() etc. earlier in sc
    ans = structure(lapply(ff, function(f) getFunctionDefs(readScript(file.path(dir, f)), envir)), names = ff)

    if(byFile)
        ans
    else if(length(ans))
        structure(unlist(ans), names = unlist(lapply(ans, names)))
    else
        list()
}

