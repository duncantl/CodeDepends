
# ideally allow the author to specify in a section that future references
# to a variable, e.g. x, are not related to the previous x, i.e.
# that this x and the one in the past (or the future) is not the same x.

# We should allow the caller to specify which sections to work on.



sourceVariable =
  #
  # evaluate all the relevant code blocks in order to 
  # define the specified variable.
  #' @param force logical value that controls whether to run a command to create a variable
  #      in one of the dependent expressions even if it already exists
  #      This allows us to skip expensive steps that have already been performed
  #' @param first is intended  to allow running up to the first instance of the variable, not all of them.  
  #
function(vars, doc, frags = readScript(doc), eval = TRUE, env = globalenv(),
         nestedEnvironments = FALSE, verbose = TRUE,
         checkLibraries = eval, force = FALSE,
         first = FALSE, info = lapply(frags, getInputs) )
{
  if(!missing(doc) && is(doc, "Script") && missing(frags))
    frags = doc
  
  idx = getVariableDepends(vars, frags, info, checkLibraries = checkLibraries, asIndex = TRUE)
  if(length(idx) == 0) {
    warning("no variable(s) named ", paste(vars, collapse = ", "), " defined in the script")
    return(FALSE)
  }
  els = frags[idx]

    # XXX allow force to be the names of variables to force recomputation of.
  if(!force) {
    info = info[idx]
    done = sapply(info, function(w) length(w@outputs) > 0 && all(sapply(w@outputs, exists, envir = env)))
    els = els[!done]
  }

  if(eval)
    invisible(evalFrags(els, env, nestedEnvironments, verbose))
  else
    els
}


############################


runUpToSection =
  #
  #    frags = parse(system.file("samples", "dual.R", package = "CodeDepends"))  
  #    runUpToSection(3, frags = frags, verbose = TRUE, all = TRUE)
  #
function(section, doc, all = TRUE, env = globalenv(), nestedEnvironments = FALSE,
         frags = readScript(doc), # , sections = seq(length = section)), #XXX doesn't apply anymore
           verbose = FALSE)
{
        if(!missing(frags))
                frags = frags[ seq_len(section) ]

        invisible(
                  if(all) {
                          evalFrags(frags, env, nestedEnvironments, verbose)
                  } else {
                          ## Figure out which bits we need to run?
                          ## XXX need to determine the dependencies.
                  })
}

evalFrags =
  #
  # Evaluate the given code blocks.
  #
function(frags, env = new.env(), nestedEnvironments = FALSE, verbose = FALSE)
{
   envs = list(env)
   ans = lapply(seq(along = frags), function(i) {
           if(nestedEnvironments)
                   e = envs[[i + 1]] <<- new.env(parent = envs[[i]])
           else
                   e = env
           if(verbose)
                   print(frags[[i]])
           eval(frags[[i]], e)
   })
   if(nestedEnvironments)
       envs
   else
       ans  
}



parseFrag =
function(node, setOptions = TRUE)
{
  if(setOptions) {
    old = options()
    on.exit(options(old))
    options(keep.source = FALSE)
  }

  parse(text = xmlValue(node))   
}



################

getLocalFunctions =
  #
  # Given the functions logical vector from a ScriptNodeInfo,
  # extract the names of the locally defined functions (TRUE), or locally and unknown (NA)
  # or the non-local ones (FALSE)
  #
function(functions, local = TRUE)
{
   if(is.null(local))
     return(character())
   
   w = if(is.na(local))
           is.na(functions) | functions
        else if(local)
           !is.na(functions) & functions
        else if(!local)
           !is.na(functions) & !functions
     
   names(functions)[w]
}

getSectionDepends =
  #' @param sect number
function(sect, frags, info = lapply(frags, getInputs, ...), index = FALSE, ...)
{
    if(missing(frags) && !index)
        stop("frags must be specified if index is FALSE")
    target = info[[sect]]
    
    ## Linear or not ? i.e. can we go forward in the document to find the definition of a var.
    ## And when we return the list here, the order may be important.
    
    inputs = c(target@inputs, getLocalFunctions(target@functions, TRUE))
    
    if(length(inputs) == 0)
        i = integer()
    else
        i = getDepends(inputs, info[1:(sect - 1)])
    
    i = c(rev(i), sect)
    if(index)
        i
    else
        frags[i]
}

getVariableDepends =
  #
  # Return the code fragments needed to define the variable(s) in vars
  # including the one that actually defines the variable.
  #
    function(vars, frags = list(), info = lapply(frags, getInputs, ...),
             checkLibraries = FALSE, asIndex = FALSE, functions = TRUE,
             ...)
{
    if(length(frags) ==0 && missing(info))
        stop("one of frags and info must be specified when calling getVariableDepends.")
    else if(missing(frags) && !asIndex)
        stop("frags must be specified if asIndex is FALSE when calling getVariableDepends")
            
    defs = sapply(info, function(v) any(vars %in% getVariables(v, functions = functions)))
    
    ans = lapply(which(defs), getSectionDepends, frags, info, TRUE, ...)
    if(length(unlist(ans)) == 0)
        return(NULL)
    
    idx = sort(unique(unlist(ans)))
    
    if(checkLibraries) {
        ## heuristic for now.
        fns = unlist(lapply(info[idx], function(x) getLocalFunctions(x@functions, NA)))
        miss = !sapply(fns, exists, mode = "function")
        if(any(miss)) {
            w = which(sapply(info[1:max(idx)], function(x) length(x@libraries) > 0))
            idx = sort(unique(c(idx, w)))
        }
    }
    
    if(asIndex)
        idx
    else
        frags[idx]
}  




###############################################

getExpressionThread =
  #
  # This finds all the expressions needed to evaluate the given variable or
  # expressions

  # target can be a variable or the index of one of the expressions.

  #
  #
  #
function(target, expressions, info = lapply(expressions, getInputs, ...), ...)
{
  if(is.integer(target)) 
    target = getVariables(info[[target]])

  idx = getDependsThread(target, info)

  expressions[sort(idx)]
}

  #
  # Get all the variables mentioned.
  # This should be generic
  #
setGeneric("getVariables", function(x, inputs = FALSE,
                                    functions = TRUE,
                                    ...) {
    standardGeneric("getVariables")
})

setMethod("getVariables", "Script", 
          function(x, inputs = FALSE, functions = TRUE,  ...)  {
    getVariables(getInputs(x, ...), inputs = inputs,
                 functions = functions, ...)
})

setMethod("getVariables", "ScriptNode",
          function(x, inputs = FALSE, functions = TRUE, ...)  {          
    getVariables(getInputs(x, ...), inputs = inputs,
                 functions = functions, ...)
         })

setMethod("getVariables", "ScriptNodeInfo",
          function(x, inputs = FALSE, functions = TRUE, ...)  {          
    c(x@outputs, x@updates, x@sideEffects,
      if(inputs) c(x@inputs, getLocalFunctions(x@functions, functions)))
})

setMethod("getVariables", "ScriptInfo",
            function(x, inputs = FALSE, functions = TRUE, ...)  {          
    unlist(lapply(x, getVariables, inputs = inputs, 
                  functions = functions, ...))
            })

setMethod("getVariables", "expression",
            function(x, inputs = FALSE, functions = TRUE, ...)  {          
    getVariables(getInputs(x, ...),
                 inputs = inputs, functions = functions, ...)
            })              


########################################
         
getPropagateChanges =
  #
  # Get the expressions which directly depend on the specified variable
  #
  # This can be used within a dynamic document in the callback for a
  # a control for a variable, e.g. sample size, which would then
  # update the dependent variables which would then update their
  # dependent expressions.
  #
  #  e = parse("../inst/examples/sim.R")
  #  getPropagateChanges("n", e, recursive = TRUE)
  #
function(var, expressions, info = lapply(expressions, getInputs), recursive = FALSE, index = FALSE,
         envir = globalenv(), eval = !missing(envir), verbose = FALSE)

{
  w = sapply(info, function(x) any(var %in% x@inputs))
  if(!recursive) {
    if(index)
      return(which(w))
    else
      return(expressions[w])
  }

  ans = which(w)

  tmp = ans
  ctr = 1
  while(TRUE) {
   tmp = lapply(tmp,
                function(i) {
                 if(i == length(info))
                   return(integer())

                  x = unlist(lapply(getVariables(info[[i]]), getPropagateChanges,
                              info = info[-(1:i)], recursive = FALSE, index = TRUE))
                 if(length(x))
                   i + x
                 else
                   x
               })
   tmp = unlist(tmp)
   if(length(tmp) == 0)
     break

   if((ctr <- ctr + 1) > 100) 
     stop("problems")
   
   tmp = unique(tmp)
   ans = c(ans, tmp)
 }

  ans = unique(ans)
  if(eval)
    return(lapply(expressions[unique(ans)],
                   function(x) {
                     if(verbose)
                       cat(x, "\n")
                     eval(x, envir)
                   }))
                       
                       
  if(index)
    ans
  else
    expressions[ans]
}  


##############################################

getDepends =
  #
  # Find the variables on which this var depends
  #
  # otherSections is the result of getInputs(), possibly subsetted.
  #
  #  Is there a graph algorithm we should use.
  # Find the paths through a graph from var to all other
  # where the graph is directed with a node from a -> b
  # indicating that b is an input to defining a.
  #
  # Used by getSectionDepends
  #
function(var, otherSections)
{
  index = integer()
  i = length(otherSections)
  while(i > 0 && length(var)) {
     src = otherSections[[i]]
     found = var %in% getVariables(src) # src@outputs & updates
      # Now have to check if some of the outputs in src$outputs
      # are actually also inputs.
     if(any(found)) {
       index = c(index, i)
         # have to be careful what we exclude. They have to be
         # only outputs, and not updates. We still need to keep looking for the updates.
         # So remove all the var[found] which are in the outputs
         # in other words, include all the var[found] that are not in the  outputs.
       var = c(var[!found], setdiff(var[found], src@outputs), src@inputs)
     }
     i = i-1
  }
  index
}  


####################################
#

getDocFragmentsByVar =
  #
  # Find the code within the specified XMLInternalDocument 
  #  which have the specified
  #
  # Mmmm.  Not used as getInputs() now returns something different.
  #
function(vars, doc, frags = xpathApply(doc, "//r:code|//r:func", function(x) parse(text = xmlValue(x))))
{
  code = sapply(frags, function(x) any(vars %in% getInputs(x)))
  frags[code]
}  
