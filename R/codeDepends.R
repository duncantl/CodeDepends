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

#########################################################################################

BuiltinFunctions =
  c("[", "[[", "$") 

inputCollector =
  #
  #  Want to be able to collect file names being source()'d and so on.
  #  Would like them to be relative to the  location of the script.
  #  Need to call isFile() with basedir correctly
  #
function(..., functionHandlers = list(...), inclPrevOutput = FALSE, checkLibrarySymbols = FALSE, funcsAsInputs = checkLibrarySymbols)
{
  libraries = character()
  if(checkLibrarySymbols)
      libSymbols = corePkgSyms
  else
      libSymbols = character()
  files = character()
  strings = character()
      # What about collecting numbers, or all literals.
  vars = character()
  set = character()
  functions = character()
  removes = character()
  updates = character()
  sideEffects = character()
  formulaVariables = character()
  
  
  Set = function(name) set <<- c(set, name)

  reset = function() {
    libraries <<- character()
    if(checkLibrarySymbols)
        libSymbols <<- corePkgSyms
    else
        libSymbols <<- character()
    
    files <<- character()
    strings <<- character()
    vars <<- character()
    set <<- character()
    functions <<- character()
    removes <<- character()
    updates <<- character()
    formulaVariables <<- character()
    sideEffects <<- character()
  }
  
  list(library = function(name)
       {
           #what about dependencies? do we want c(libraries, getDeps(name), name)?
           libraries <<- c(libraries, name)
           if(checkLibrarySymbols)
               libSymbols <<- c(libSymbols, librarySymbols(name))
       },
       addInfo = function(funcNames = character(), modelVars = character()) {
           formulaVariables <<- c(formulaVariables,  modelVars)
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
       vars = function(name, input) {
                if(!length(name))
                  return()
                if(input)
                  {
                    if(inclPrevOutput)
                      vars <<- c(vars, name[ !( name %in% c(BuiltinFunctions, libSymbols) ) ] ) #BuiltinFunctions ) ])  # || name %in% set
                    else
                  ##Variables can't be an input if they are already an output ~GB
                      vars <<- c(vars, name[ !( name %in% c(BuiltinFunctions, set, libSymbols) ) ] ) #BuiltinFunctions  || name %in% set )])  # || name %in% set

                  }
                else
                   Set(name)
              },
       set = Set,
       calls = function(name) {
           functions <<- c(functions, name)
           
           if(funcsAsInputs)
               vars(name, TRUE)
       },
       
       removes = function(name) removes <<- c(removes, name),
       sideEffects = function(name) sideEffects <<- c(sideEffects, name),       
       functionHandlers = functionHandlers,
       reset = reset,
#       addInfo = addInfo,
       results = function(resetState = FALSE) {
                      ans = new("ScriptNodeInfo",
                                 libraries = unique(libraries),
                                 files = unique(files),
                                 strings = unique(strings),         
                                 inputs = unique(vars),
                                 outputs = unique(set),
                                 updates = unique(updates),
                                 removes = removes,
                                 formulaVariables = formulaVariables,
                                 functions = unique(functions),
                                 sideEffects = unique(sideEffects))
                      
                      if(resetState) 
                        reset()
                      ans
                    })
}  


setGeneric("getInputs",
           function(e, collector = inputCollector(), basedir = ".", reset = FALSE, ...) {
             standardGeneric("getInputs")
           })

getInputs.language =          
function(e, collector = inputCollector(), basedir = ".", reset = FALSE, input = TRUE, formulaInputs = FALSE, ...)
{
  ans = character()
  update = FALSE

  if(inherits(e, "expression")) {

     ans = lapply(e, getInputs, collector = collector, basedir = basedir, formulaInputs = formulaInputs, ...)

  } else if(is.function(e)) {

     ans = codetools::findGlobals(e, FALSE)
     collector$vars(ans$variables, input = TRUE)
     collector$calls(ans$functions)     

  } else if(is.call(e)) {

     findSideEffects(e, collector)
     #put the customized handler check first so that it can override  default behaviors ~GB
     if(is.symbol(e[[1]]) && as.character(e[[1]]) %in% names(collector$functionHandlers)) {
         collector$functionHandlers[[ as.character(e[[1]]) ]](e, collector, basedir)
     } else if(is.symbol(e[[1]]) && as.character(e[[1]]) == "function") {
       tmp = eval(e)
       ans = codetools::findGlobals(tmp, FALSE)
       collector$vars(ans$variables, input = TRUE)
       collector$calls(ans$functions)
     } else if(is.symbol(e[[1]]) && as.character(e[[1]]) == "~") {
         # a formula, eg a~b
         # whether we count variables that appear in formulas as inputs for the expression is controlled by the formulaInputs paramter
       #eventually we want to be able to handle the situation where we are calling
       #lm(y~x + z, data=dat) where y and x are in dat but z is not, but that is HARD to detect so for now we allow users to specify whether CodeDepends counts all variables used by formulas (assuming they come from the global environment/current scope) or none (assuming the fomula will be used only within the scope of, eg, a data.frame). I think the second one is the most common use-case in practice...
       collector$call(as.character(e[[1]]))
       if(formulaInputs)
           lapply(e[-1], getInputs, collector, basedir = basedir, input = input, formulaInputs = formulaInputs, ...)
       else {
          # collect the variables and functions in the 
         col = inputCollector()
         lapply(e[-1], getInputs, col, basedir = basedir, input = input, formulaInputs = formulaInputs, ...)
         vals = col$results()
         collector$addInfo(modelVars = vals@inputs, funcNames = vals@functions)
       }

     } else if(is.symbol(e[[1]]) && as.character(e[[1]]) %in% c("require", "library")) {

         # XXX Deal with case there are more arguments and also that the
         # first argument is not the name of the library and that it is a
         # a character.
         # Should also deal with match named arguments!
       collector$library(as.character(e[[2]]))

     } else if(is.symbol(e[[1]]) && as.character(e[[1]]) %in% c("rm")) {
        collector$removes( sapply(e[-1], as.character) )

     } else if(is.symbol(e[[1]]) && as.character(e[[1]]) %in% c("$")) {
        collector$vars(as.character(e[[2]]), input = input)
    
     } else {

            # an assignment.
       if(is.symbol(e[[1]]) && as.character(e[[1]]) %in% c("<-", "=", "<<-")) {
            # Do the left hand side first.
            #if it is a simple name, then it is an output,
            # but otherwise it is a call and may have more inputs.
           if(!is.name(e[[2]])) {

                  # if this is a x$foo <- val or x[["foo"]] = val or x[i, j] <- val
                  # make certain to add the variable being updated as an input.
                  # It will also be an output. 
              if(is.name(e[[2]][[2]])) {
                if(FALSE) {
                   if(TRUE || as.character(e[[2]][[1]]) %in% c("$", "[", "[["))
                      collector$vars(asVarName(e[[2]][[2]]), input = input)
                   collector$set(asVarName(e[[2]][[2]]))
                 }

                 collector$update(asVarName(e[[2]][[2]]))
                 collector$call(paste(as.character(e[[2]][[1]]), "<-", sep = ""))
                 update = TRUE
              }

              if(as.character(e[[2]][[1]]) != "$")
                lapply(as.list(e[[2]][-c(1,2)]), getInputs, collector, basedir = basedir, input = FALSE, formulaInputs = formulaInputs, ...)
           }

             # Do the right hand side
           lapply(e[-c(1,2)], getInputs, collector, basedir = basedir, input = TRUE, formulaInputs = formulaInputs)

            if(is.name(e[[2]]))
               collector$set(asVarName(e[[2]]))
             else {
               # handle, e.g.
               #   foo(x) = 1
               #   x[["y"]] = 2
               #   x [ x > 0 ] = 2 * y
               if(is.call(e[[2]])) {
                       #XXX will get foo in foo(x)
                   if(!update)
                      collector$set(asVarName(e[[2]][[2]]))
                   if(as.character(e[[2]][[1]]) != "$")                   
                     lapply(e[[2]][-c(1,2)], getInputs, collector, basedir = basedir, input = input, formulaInputs = formulaInputs, ...)
               } else {
                   collector$set(asVarName(e[[2]][[2]]))
               }
             }

       } else if(!(is.symbol(e[[1]]) && as.character(e[[1]]) == "function")) {
           collector$call(as.character(e[[1]]))           
           lapply(e[-1], getInputs, collector, basedir = basedir, input = input, formulaInputs = formulaInputs, ...)
       } else if(is.symbol(e[[1]])) {
          collector$call(as.character(e[[1]]))
          lapply(e[-1], getInputs, collector, basedir = basedir, input = input, formulaInputs = formulaInputs, ...)          
       }
     }

  } else if(is.name(e) || is.symbol(e)) {

     if(as.character(e) != "")
       collector$vars(as.character(e), input)

  } else if(is.integer(e) || is.logical(e) || is.numeric(e) || is.complex(e)) {
      # literal so ignore.
    
  }  else if(is.character(e)) {
     collector$string(e, basedir = basedir)
     
#    if(file.exists(e) || file.exists(paste(basedir, e, sep = .Platform$file.sep)))
#      collector$file(e)
#    else
#      collector$string(e)

   } else if(is.pairlist(e)) {

     lapply(e, getInputs, collector = collector, basedir = basedir, input = input, formulaInputs = formulaInputs, ...)

   } else {

     stop("don't know about ", class(e))

   }
  
 collector$results(reset = reset)
}

#setMethod("getInputs", "expression", getInputs.language)
#setMethod("getInputs", "call", getInputs.language)
#setMethod("getInputs", "{", getInputs.language)
#setMethod("getInputs", "=", getInputs.language)
setMethod("getInputs", "ANY", getInputs.language)

setMethod("getInputs", "Script",
function(e, collector = inputCollector(), basedir = ".", reset = FALSE, ...)
{
  ans = lapply(e, getInputs, collector = collector,  basedir = basedir, reset = TRUE, ...)
  new("ScriptInfo", ans)
})

setMethod("getInputs", "ScriptNode",
function(e, collector = inputCollector(), basedir = ".", reset = FALSE, ...)
{
  getInputs(e@code, collector, basedir, ...)
})


setMethod("getInputs", "ScriptNodeInfo",
function(e, collector = inputCollector(), basedir = ".", reset = FALSE, ...)
{
  e
})



setMethod("getInputs", "function",
            function(e, collector = inputCollector(), basedir = ".", reset = FALSE, ...) {
              expr = body(e)
              if(as.character(expr[[1]]) == "{")
                 expr = expr[-1]
              vars = new("ScriptNodeInfo", outputs = names(formals(e)))
              new("ScriptInfo", c(vars, lapply(expr, getInputs, collector = collector, basedir = basedir)))
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


