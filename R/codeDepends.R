########################################################################################
asVarName =
function(x)
{
  if(is.call(x)) {
    x = asVarName(x[[2]])
  }

 as.character(x)
}


#
isFile =
function(val, basedir = ".")
  file.exists(val) || file.exists(paste(basedir, val, sep = .Platform$file.sep))  

is.native = function(x) inherits(x, "NativeSymbol")

#########################################################################################

BuiltinFunctions =
  c("[", "[[", "$") 

inputCollector =
  #
  #  Want to be able to collect file names being source()'d and so on.
  #  Would like them to be relative to the  location of the script.
  #  Need to call isFile() with basedir correctly
  #
    function(..., functionHandlers = list(...), inclPrevOutput = FALSE,
             checkLibrarySymbols = FALSE,
             funcsAsInputs = checkLibrarySymbols)
{
    cust = names(functionHandlers)
    functionHandlers = c(functionHandlers,
                         defaultFuncHandlers[! names(defaultFuncHandlers) %in% cust])
    libraries = character()
    if(checkLibrarySymbols)
        libSymbols = corePkgSyms
    else
        libSymbols = character()
    files = character()
    strings = character()
    ## What about collecting numbers, or all literals.
    ## this is the inputs  
    vars = character()
    ## this is the outputs
    set = character()
    functions = character()
    removes = character()
    updates = character()
    sideEffects = character()
    nsevalVars = character()
    code = NULL
    ## this one doesn't get blown away by reset, "libraries" still does 
    ## we need this to handle dplyr, etc based code in a script, because
    ## the script getInput methods lapplys getInputs with reset=TRUE, so
    ## we lose package info outside of the individual expression.
    ##
    ## FIXME we need a better solution to design mismatch generally, I think . ~G
    allpackages = libraries
    
    Set = function(name) {
            set <<- c(set, name)
        }

    Vars =  function(name, input) {
        if(!length(name))
            return()
        if(input)
        {
            if(inclPrevOutput)
                vars <<- c(vars, name[ !( name %in% c(BuiltinFunctions, libSymbols) ) ] ) #BuiltinFunctions ) ])  # || name %in% set
            else
                ##Variables can't be an input if they are already an output ~GB
                vars <<- c(vars, name[ !( name %in% c(BuiltinFunctions, set, libSymbols) ) ] )             
        }
        else
            Set(name)
    }
    reset = function(full = FALSE) {
        libraries <<- character()
        if(full) {
            if(checkLibrarySymbols)
                libSymbols <<- corePkgSyms
            else
                libSymbols <<- character()
            allpackages <<- character()
        }
        files <<- character()
        strings <<- character()
        vars <<- character()
        set <<- character()
        functions <<- character()
        removes <<- character()
        updates <<- character()
        nsevalVars <<- character()
        sideEffects <<- character()
        code <<- NULL
    }
  
  list(library = function(name)
       {
           #what about dependencies? do we want c(libraries, getDeps(name), name)?
           libraries <<- c(libraries, name)
           if(checkLibrarySymbols)
               libSymbols <<- c(libSymbols, librarySymbols(name))
           allpackages <<- c(allpackages, libraries)
       },
       addInfo = function(funcNames = character(), modelVars = character()) {
           nsevalVars <<- c(nsevalVars,  modelVars)
           functions <<- c(functions, funcNames)
       },
       string = function(name, basedir = NA, filep = isFile(name, basedir))
                if(filep)
                    files <<- c(files, name)
                else
                    strings <<- c(strings, name),
#       string = function(name) strings <<- c(strings, name),       
       update = function(name) {
                   if(!length(name))
                      return()
                   updates <<- c(updates, name)
                  },
       vars = Vars,
       set = Set,
       calls = function(name) {
           functions <<- c(functions, name)
           
           if(funcsAsInputs) {
               Vars(name, TRUE)
           }
       },
       
       removes = function(name) removes <<- c(removes, name),
       sideEffects = function(name) sideEffects <<- c(sideEffects, name),       
       functionHandlers = functionHandlers,
       reset = reset,
       code = function(name) code <<- name,
       nsevals = function(name) nsevalVars <<- c(nsevalVars, name),
       pkgLoadHistory = function() allpackages, 
#       addInfo = addInfo,
       results = function(resetState = FALSE) {
                      funcs = unique(functions)
                      ans = new("ScriptNodeInfo",
                                 libraries = unique(libraries),
                                 files = unique(files),
                                 strings = unique(strings),         
                                 inputs = unique(vars),
                                 outputs = Unique(set),
                                 updates = unique(updates),
                                 removes = removes,
                                 nsevalVars = nsevalVars,
                                 functions = structure(rep(NA, length(funcs)), names = funcs),
                                 sideEffects = unique(sideEffects),
                                code = code)
                      
                      if(resetState) 
                        reset()
                      ans
  },
  collectorSettings = function() {
      list(functionHandlers = functionHandlers,
           inclPrevOutputs = inclPrevOutput,
           checkLibrarySymbols = checkLibrarySymbols,
           funcsAsInputs = funcsAsInputs)
  })
                                       
                                       
}

Unique =
function(x)
{
    x[!duplicated(x)]
}


setGeneric("getInputs",
           function(e, collector = inputCollector(), basedir = ".",
                    reset = FALSE, formulaInputs = FALSE, ...) {
    standardGeneric("getInputs")
})

getInputs.language =          
    function(e, collector = inputCollector(), basedir = ".",
             reset = FALSE, formulaInputs = FALSE, input = TRUE,  ...,
             pipe = FALSE, update = FALSE, nseval=FALSE)
{
    ## scoping state hackery
    if(is.null(dynGet("getinputstoplevel", ifnotfound = NULL))) {
        if(reset)
            collector$reset()
        collector$code(e)
        getinputstoplevel = TRUE
    }

  ans = character()

  if(inherits(e, "expression")) {

     ans = lapply(e, getInputs, collector = collector, basedir = basedir,
         formulaInputs = formulaInputs, pipe = pipe, nseval = nseval, ...)

  } else if(is.function(e)) {

     ans = codetools::findGlobals(e, FALSE)
     collector$vars(ans$variables, input = TRUE)
     collector$calls(ans$functions)     

  } else if(is.call(e)) {
      if(is.symbol(e[[1]]) && nseval) {
          collector$addInfo( funcNames= as.character(e[[1]]))
          lapply(e[-1], getInputs, collector = collector, basedir =basedir,
                 formulaInputs = formulaInputs, ..., update = FALSE,
                 input = input, pipe = pipe, nseval=TRUE)
      } else {

          findSideEffects(e, collector)
          ## put the customized handler check first so that it can override  default behaviors ~GB
          ## all the built in special cases are now factored out as default handlers. See
          ## functionHandlers.R ~GB
          if(is.symbol(e[[1]]) && as.character(e[[1]]) %in% names(collector$functionHandlers)) {
              collector$functionHandlers[[ as.character(e[[1]]) ]](e, collector,
                                                                   basedir = basedir,
                                                                   formulaInputs = formulaInputs,
                                                                   update = update,
                                                                   input = input,
                                                                   pipe = pipe,
                                                                   nseval = nseval)
          } else {
              collector$functionHandlers[["_default_"]](e, collector,
                                                       basedir = basedir,
                                                       formulaInputs = formulaInputs,
                                                       update = update,
                                                       input = input,
                                                       pipe = pipe,
                                                       nseval = nseval)
              
       
          }
      }
     
 } else if(isAssignment(e)) {
     collector$functionHandlers[["_assignment_"]](e, collector, input = input, basedir = basedir,
                                                 formulaInputs = formulaInputs,
                                                 update = update, pipe = pipe,
                                                 nseval = nseval)
     
 } else if(is.name(e) || is.symbol(e)) {

     if(as.character(e) != "") {
         if(update)
             collector$update(as.character(e))
         else if (nseval)
             collector$addInfo(modelVars = as.character(e))
         else
             collector$vars(as.character(e), input)
     }

  } else if(is.integer(e) || is.logical(e) || is.numeric(e) || is.complex(e)) {
      # literal so ignore.
    
  }  else if(is.character(e)) {
     collector$string(e, basedir = basedir)
     
#    if(file.exists(e) || file.exists(paste(basedir, e, sep = .Platform$file.sep)))
#      collector$file(e)
#    else
#      collector$string(e)

   } else if(is.pairlist(e)) {

     lapply(e, getInputs, collector = collector, basedir = basedir, input = input,
            formulaInputs = formulaInputs, ..., update = update, pipe = pipe,
            nseval = nseval)
   } else if(is.native(e)) {
       collector$functionHandlers[["_InlineNativeSymbol_"]](e, collector,
           input = input, basedir = basedir,
           formulaInputs = formulaInputs,
           update = update, pipe = pipe,
           nseval = nseval) 
       
   } else {

     stop("don't know about ", class(e))

   }
  
 collector$results(resetState = reset)
}

#setMethod("getInputs", "expression", getInputs.language)
#setMethod("getInputs", "call", getInputs.language)
#setMethod("getInputs", "{", getInputs.language)
#setMethod("getInputs", "=", getInputs.language)
setMethod("getInputs", "ANY", getInputs.language)

setMethod("getInputs", "Script",
function(e, collector = inputCollector(), basedir = ".", reset = FALSE, formulaInputs = FALSE, ...)
{
  
  ans = lapply(e, getInputs, collector = collector,  basedir = basedir, reset = TRUE, formulaInputs = formulaInputs, ...)
  
  new("ScriptInfo", identifyLocalFunctions(ans))
})

setMethod("getInputs", "ScriptNode",
function(e, collector = inputCollector(), basedir = ".", reset = FALSE, formulaInputs = FALSE, ...)
{
    nodes = getInputs(e@code, collector, basedir, reset = reset, formulaInputs = formulaInputs, ...)
    ## Now determine if the functions are locally defined or not, i.e. within earlier tasks in this script.
  ## We don't have the whole script here, only a single node, can't (??) do the
  ## described above. ~GB
  ## identifyLocalFunctions(nodes)
  nodes
})

identifyLocalFunctions =
function(nodes)
{
  defs = character()
  for(i in seq(along = nodes)) {
     tmp = nodes[[i]]
     if(length(tmp@functions) && any(w <- is.na(tmp@functions))) {
        tmp@functions[w] = names(tmp@functions[w]) %in% defs
        nodes[[i]] = tmp
     }
     defs = c(defs, tmp@outputs)
  }
  nodes
}


setMethod("getInputs", "ScriptNodeInfo",
function(e, collector = inputCollector(), basedir = ".", reset = FALSE, formulaInputs = FALSE, ...)
{
  e
})



setMethod("getInputs", "function",
            function(e, collector = inputCollector(), basedir = ".", reset = FALSE, formulaInputs = FALSE, ...) {
           
    ## create dummy node which declares the formal arguments so they
    ## don't show up as globals later. the call itself "outputs" these
    ## "variables"
    vars = new("ScriptNodeInfo", outputs = names(formals(e)))
    exprs = as.list(body(e))
    ## do we really want to keep the "{" around? Currently a test fails
    ## if we remove it (length of output) but the test could be changed...
    if(is.name(exprs[[1]]) && as.character(exprs[[1]]) == "{") {
        scr = readScript(txt = exprs)
    } else {
        scr = body(e)
    }

    new("ScriptInfo", c(vars, getInputs(scr, collector = collector, basedir = basedir, reset = reset, formulaInputs = formulaInputs, ...)))
    
})



####################################################

getDependCode =
  #
  # Determines which preceding blocks are used to determine
  # the inputs to the specified block. In other words, if we
  # have a 10 blocks and we call
  #   getDependCode(5, "x", 
  #
  #
  # Have to worry about when a variable is redefined.
  #
  # This is garbage!
  #
  #
function(block, depends, codeBlocks)  
{
  if(is(block, "numeric")) {
     codeBlocks = codeBlocks[seq(1, length = block)]
     depends = depends[seq(1, length = block)]     
     block =  depends[[block]]
  }

  vars = block$inputs

  ans = integer() 
  sapply(depends, function(x) vars %in% x$inputs) #???

  sort(ans)
}


############################

# Old

############################################
getAssigns =
function(e, globalsOnly = FALSE)
{
 if(inherits(e, "expression"))
   return(unique(unlist(lapply(e, getLanguageAssigns))))
 else if(is.function(e)) {
   return(getAssigns(body(e), globalsOnly = TRUE))
 }

 stop("don't know what to do")
}  

getLanguageAssigns =
function(e)
{
  ans = character()
  if(is.call(e) &&  as.character(e[[1]]) %in% c("=", "<-", "<<-")) {
     ans = c(ans, asVarName(e[[2]]))
  }
  if(length(ans) > 2)
    ans = unlist(c(ans, lapply(e[- c(1, 2)], getLanguageAssigns)))
  
  return(unique(ans))
}


#
if(FALSE) {
  doc = xmlParse("read.xml")
  xpathApply(doc, "//r:function", function(x) eval(parse(text = xmlValue(x)), globalenv()))
  code = xpathApply(doc, "//r:code|//r:plot", function(x) parse(text = xmlValue(x)))
}
#
#


