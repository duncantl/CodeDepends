

libreqhandler = function(e, collector, basedir, input, formulaInputs, update, pipe = FALSE,  ...)  collector$library(as.character(e[[2]]))

rmhandler = function(e, collector, basedir, input, formulaInputs, update, pipe = FALSE, ...) collector$removes(sapply(e[-1], as.character))

dollarhandler = function(e, collector, basedir, input, formulaInputs, update, pipe = FALSE, ...) collector$vars(as.character(e[[2]]), input = input)

assignhandler = function(e, collector, basedir, input, formulaInputs, update, pipe = FALSE, ...) {
    ## Do the left hand side first.
    ##if it is a simple name, then it is an output,
    ## but otherwise it is a call and may have more inputs.
    if(!is.name(e[[2]])) {
        
        ## if this is a x$foo <- val or x[["foo"]] = val or x[i, j] <- val
        ## make certain to add the variable being updated as an input.
        ## It will also be an output. 
        if(is.name(e[[2]][[2]]) || TRUE) { #XX Check - add TRUE to do this unconditionally.
            if(FALSE) {
                if(TRUE || as.character(e[[2]][[1]]) %in% c("$", "[", "[["))
                    collector$vars(asVarName(e[[2]][[2]]), input = input)
                collector$set(asVarName(e[[2]][[2]]))
            }
            collector$update(asVarName(e[[2]][[2]]))
            collector$call(paste(as.character(e[[2]][[1]]), "<-", sep = ""))
            ## needs to modify getInputs state. a bit of a sharp edge for the refactor ~GB
            update <<- TRUE
        }
        
        if(as.character(e[[2]][[1]]) != "$")
            lapply(as.list(e[[2]][-c(1,2)]), getInputs, collector, basedir = basedir, input = FALSE, formulaInputs = formulaInputs, ..., update = update, pipe= pipe)
    }

    ## Do the right hand side
    lapply(e[-c(1,2)], getInputs, collector, basedir = basedir, input = TRUE, formulaInputs = formulaInputs, update = FALSE, pipe = pipe)
    
    if(is.name(e[[2]])) 
        collector$set(asVarName(e[[2]]))
    else {
        ## handle, e.g.
        ##   foo(x) = 1
        ##   x[["y"]] = 2
        ##   x [ x > 0 ] = 2 * y
        if(is.call(e[[2]])) {
            ##XXX will get foo in foo(x)
            if(!update)
                collector$set(asVarName(e[[2]][[2]]))
            if(as.character(e[[2]][[1]]) != "$")                   
                lapply(e[[2]][-c(1,2)], getInputs, collector, basedir = basedir, input = input, formulaInputs = formulaInputs, ..., update = update, pipe = pipe)
        } else {
            collector$set(asVarName(e[[2]][[2]]))
        }
    }
}

funchandler = function(e, collector, basedir, input, formulaInputs, update, pipe = FALSE, ...){
    tmp = eval(e)
    ans = codetools::findGlobals(tmp, FALSE)
    collector$vars(ans$variables, input = TRUE)
    collector$calls(ans$functions)
}


formulahandler =  function(e, collector, basedir, input, formulaInputs, update, pipe = FALSE, ...){
    
    ## a formula, eg a~b
    ## whether we count variables that appear in formulas as inputs for the expression is controlled by the formulaInputs paramter
    ##eventually we want to be able to handle the situation where we are calling
    ##lm(y~x + z, data=dat) where y and x are in dat but z is not, but that is HARD to detect so for now we allow users to specify whether CodeDepends counts all variables used by formulas (assuming they come from the global environment/current scope) or none (assuming the fomula will be used only within the scope of, eg, a data.frame). I think the second one is the most common use-case in practice...
    collector$call(as.character(e[[1]]))
       if(formulaInputs)
           lapply(e[-1], getInputs, collector, basedir = basedir, input = input, formulaInputs = formulaInputs, ..., update = update, pipe = pipe)
       else {
                                        # collect the variables and functions in the 
           col = inputCollector()
         lapply(e[-1], getInputs, col, basedir = basedir, input = input, formulaInputs = formulaInputs, ..., update = update, pipe = pipe)
         vals = col$results()
         ## format of vals@functions is named vector of NA with functions as the names
         ## logical value in return appears to indicate local or not.
         collector$addInfo(modelVars = vals@inputs, funcNames = names(vals@functions))
     }
       
}



assignfunhandler = function(e, collector, basedir, input, formulaInputs, update, pipe = FALSE, ...){
    if(is.symbol(e[[2]])) {
        warning("assign() used with symbol as first argument. Unable to statically resolve what name the value will be assigned to")
        return()
    } else { ## character containing the name to assign to
        collector$calls("assign")
        collector$set(e[[2]]) ##varable
        getInputs(e[[3]], collector = collector, basedir = basedir, input = TRUE,
                  formulaInputs = formulaInputs, update = update, pipe = pipe, ... )
    }

}

fullnsehandler = function(e, collector, basedir, input, formulaInputs, update, pipe = FALSE, ...) {
    collector$calls(as.character(e[[1]]))
}

nseafterfirst = function(e, collector, basedir, input, formulaInputs, update,
    pipe = FALSE, ...) {
     collector$calls(as.character(e[[1]]))
     if(!pipe)
         getInputs(e[[2]],  collector = collector, basedir = basedir, input = TRUE,
                   formulaInputs = formulaInputs, update = update, pipe = FALSE, ...)
 }

filterhandler = function(e, collector, basedir, input, formulaInputs, update, pipe = FALSE, ...) {
    if("dplyr" %in% collector$results()@libraries)
        nseafterfirst(e, collector, basedir = basedir, input = input,
                      formulaInputs = formulaInputs, update = update,
                      pipe = pipe, ...)
    else {
        collector$calls("filter")
        lapply(e[-1], getInputs, col, basedir = basedir, input = input, formulaInputs = formulaInputs, ..., update = update, pipe = pipe)
    }
}

pipehandler = function(e, collector, basedir, input, formulaInputs, update,
    pipe = FALSE, ...) {
    collector$calls("%>%")
    ## right-hand operand of %>%, always a function symbol or function call
    if(is.symbol(e[[3]]))
        collector$calls(as.character(e[[3]]))
    else 
        getInputs(e[[3]], collector = collector, basedir = basedir, input = TRUE, formulaInputs = formulaInputs, update = update, pipe = TRUE)
    ## left hand side. leaf only if we're to the start of the expr,
    ## which won't be a function
    if(is.symbol(e[[2]])) 
        collector$vars(as.character(e[[2]]), input=TRUE)
    else
        getInputs(e[[2]], collector = collector, basedir = basedir, input = TRUE, formulaInputs = formulaInputs, update = update, pipe = TRUE)

}

defaultFuncHandlers = list(
    library = libreqhandler,
    require = libreqhandler,
    requireNamespace = libreqhandler,
    rm = rmhandler,
    "$" = dollarhandler,
    "=" = assignhandler,
    "<-" = assignhandler,
    "<<-" = assignhandler,
    "function" = funchandler,
    "~" = formulahandler,
    "assign" = assignfunhandler,
    "_assignment" = assignhandler,
    aes = fullnsehandler,
    subset = nseafterfirst,
    transform = nseafterfirst,
    filter = filterhandler,
    "%>%" = pipehandler
    
    
    )

isAssignment = function(e) class(e) %in% c("=", "<-") || (is.call(e) && is.symbol(e[[1]]) && as.character(e[[1]]) == "<<-")
