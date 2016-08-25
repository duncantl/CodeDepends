

libreqhandler = function(e, collector, basedir, input, formulaInputs, update, pipe = FALSE,  ...)  collector$library(as.character(e[[2]]))

rmhandler = function(e, collector, basedir, input, formulaInputs, update, pipe = FALSE, nseval = FALSE, ...) collector$removes(sapply(e[-1], as.character))

dollarhandler = function(e, collector, basedir, input, formulaInputs, update, pipe = FALSE, nseval = FALSE, ...) {
    


    ##need to handle cases like a$b$c, which translate to `$`(a$b, c), correctly.
    ## Only a is a real variable here! Identified based on MathiasHinz
    ## https://github.com/duncantl/CodeDepends/issues/4
    if(is(e[[2]], "name"))
        collector$vars(as.character(e[[2]]), input = input)
    else
        getInputs(e[[2]], collector = collector, basedir = basedir, input = input, formulaInputs = formulaInputs,
                  update = update, pipe = pipe, nseval = nseval, ...)
}



assignhandler = function(e, collector, basedir, input, formulaInputs, update, pipe = FALSE, nseval = FALSE, ...) {
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
            update = TRUE
        }
        
        if(as.character(e[[2]][[1]]) != "$")
            lapply(as.list(e[[2]][-c(1,2)]), getInputs, collector, basedir = basedir, input = FALSE, formulaInputs = formulaInputs, ..., update = update, pipe= pipe, nseval = nseval)
    }

    ## Do the right hand side
    lapply(e[-c(1,2)], getInputs, collector, basedir = basedir, input = TRUE, formulaInputs = formulaInputs, update = FALSE, pipe = pipe, nseval = nseval)
    
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
                lapply(e[[2]][-c(1,2)], getInputs, collector, basedir = basedir, input = input, formulaInputs = formulaInputs, ..., update = update, pipe = pipe, nseval = nseval)
        } else {
            collector$set(asVarName(e[[2]][[2]]))
        }
    }
}

funchandler = function(e, collector, basedir, input, formulaInputs, update, pipe = FALSE, nseval = FALSE, ...){
    tmp = eval(e)
    ans = codetools::findGlobals(tmp, FALSE)
    collector$vars(ans$variables, input = TRUE)
    collector$calls(ans$functions)
}


formulahandler =  function(e, collector, basedir, input, formulaInputs, update, pipe = FALSE, nseval = FALSE, ...){
    
    ## a formula, eg a~b
    ## whether we count variables that appear in formulas as inputs for the expression is controlled by the formulaInputs paramter
    ##eventually we want to be able to handle the situation where we are calling
    ##lm(y~x + z, data=dat) where y and x are in dat but z is not, but that is
    ## HARD to detect so for now we allow users to specify whether CodeDepends
    ##counts all variables used by formulas (assuming they come from the global
    ##environment/current scope) or none (assuming the fomula will be used only
    ##within the scope of, eg, a data.frame). I think the second one is the most
    ##common use-case in practice...

    ## XXX port this over to the new way of dealing with nseval? ~GB
    collector$call(as.character(e[[1]]))
       if(formulaInputs)
           lapply(e[-1], getInputs, collector, basedir = basedir, input = input, formulaInputs = formulaInputs, ..., update = update, pipe = pipe, nseval = nseval)
       else {
                                        # collect the variables and functions in the 
           col = inputCollector()
         lapply(e[-1], getInputs, col, basedir = basedir, input = input,
                formulaInputs = formulaInputs, ..., update = update, pipe = pipe,
                nseval = nseval)
         vals = col$results()
         ## format of vals@functions is named vector of NA with functions as the names
         ## logical value in return appears to indicate local or not.
         collector$addInfo(modelVars = vals@inputs, funcNames = names(vals@functions))
     }
       
}



assignfunhandler = function(e, collector, basedir, input, formulaInputs, update, pipe = FALSE, nseval = FALSE, ...){
    if(is.symbol(e[[2]])) {
        warning("assign() used with symbol as first argument. Unable to statically resolve what name the value will be assigned to")
        return()
    } else { ## character containing the name to assign to
        collector$calls("assign")
        collector$set(e[[2]]) ##varable
        getInputs(e[[3]], collector = collector, basedir = basedir, input = TRUE,
                  formulaInputs = formulaInputs, update = update, pipe = pipe,
                  nseval = nseval, ... )
    }

}

fullnsehandler = function(e, collector, basedir, input, formulaInputs, update, pipe = FALSE, nseval = FALSE, ...) {
    collector$calls(as.character(e[[1]]))
    lapply(e[-1], getInputs, collector = collector, basedir = basedir,
            input = TRUE, formulaInputs = formulaInputs, update = update,
            pipe = pipe, nseval = TRUE)
}

nseafterfirst = function(e, collector, basedir, input, formulaInputs, update,
    pipe = FALSE, nseval = FALSE, ...) {
     collector$calls(as.character(e[[1]]))
     if(!pipe) {
         ## first argument
         getInputs(e[[2]],  collector = collector, basedir = basedir, input = TRUE,
                   formulaInputs = formulaInputs, update = update, pipe = FALSE,
                   nseval = FALSE, ...)
         nseseq = seq(along = e)[-c(1:2)]
     } else {
         nseseq = seq(along = e)[-1]
     }
     lapply(e[nseseq], getInputs, collector = collector, basedir = basedir,
            input = TRUE, formulaInputs = formulaInputs, update = update,
            pipe = pipe, nseval = TRUE)
 }

nsehandlerfactory = function(secount) {
    function(e, collector, basedir, input, formulaInputs, update, pipe = FALSE, nseval = FALSE, ...) {
        if(secount == 0)
            return(fullnsehandler)
        seargs = 2:(1+secount)
        if(pipe)
            seargs = seargs[-1] #pipe gobbles the first arg
        lapply(seargs, function(i) getInputs(e[[i]], collector = collector,
                                             basedir = basedir, input = input,
                                             formulaInputs = formulaInputs,
                                             update = update, pipe = pipe,
                                             nseval = FALSE, ...))
        lapply(e[-seq(1, max(seargs +1))], getInputs, collector = collector,
               basedir = basedir, input = input, formulaInputs = formulaInputs,
               update = update, pipe = pipe, nseval = TRUE, ...)
    }
}


filterhandler = function(e, collector, basedir, input, formulaInputs, update, pipe = FALSE, nseval = FALSE, ...) {
    if("dplyr" %in% collector$results()@libraries)
        nseafterfirst(e, collector, basedir = basedir, input = input,
                      formulaInputs = formulaInputs, update = update,
                      pipe = pipe, nseval = nseval, ...)
    else {
        collector$calls("filter")
        lapply(e[-1], getInputs, col, basedir = basedir, input = input, formulaInputs = formulaInputs, ..., update = update, pipe = pipe, nseval = FALSE)
    }
}

pipehandler = function(e, collector, basedir, input, formulaInputs, update,
    pipe = FALSE, nseval=FALSE, ...) {
    collector$calls("%>%")
    ## right-hand operand of %>%, always a function symbol or function call
    if(is.symbol(e[[3]]))
        collector$calls(as.character(e[[3]]))
    else 
        getInputs(e[[3]], collector = collector, basedir = basedir, input = TRUE, formulaInputs = formulaInputs, update = update, pipe = TRUE, nseval = nseval)
    ## left hand side. leaf only if we're to the start of the expr,
    ## which won't be a function
    if(is.symbol(e[[2]])) 
        collector$vars(as.character(e[[2]]), input=TRUE)
    else
        ## pipe=false because if this is a call, nothing is being passed to it
        ## via the pipe, because this is the start
        getInputs(e[[2]], collector = collector, basedir = basedir, input = TRUE, formulaInputs = formulaInputs, update = update, pipe = FALSE, nseval = nseval)

}

defhandler = function(e, collector, basedir, input, formulaInputs, update, pipe = FALSE, nseval = FALSE, ...) {
    if(is.symbol(e[[1]])) {
        collector$calls(as.character(e[[1]]))
        lapply(e[-1], getInputs, collector=collector, basedir = basedir,
               formulaInputs = formulaInputs, ..., update = update,
               input = input, pipe = pipe, nseval = nseval)

    } else if(is.call(e[[1]]) && as.character(e[[1]][[1]]) %in% c("::", ":::"))
      { ## case of :: or :::, etc
        getInputs(e[[1]], collector = collector, basedir = basedir,
                  input = input, formulaInputs = formulaInputs,
                  update = update, pipe = pipe, nseval = nseval,
                  ...)
        e2 = e
        e2[[1]] = e2[[1]][[3]] # in :: and ::: calls, 1 is the colons, 2 is lib, 3 is fun
        getInputs(e2, collector = collector, basedir = basedir,
                  input = input, formulaInputs=  formulaInputs,
                  update = update, pipe = pipe, nseval = nseval,
                  ...)
    } else {
        lapply(e, getInputs, collector=collector, basedir = basedir,
               formulaInputs = formulaInputs, ..., update = update,
               input = input, pipe = pipe, nseval = nseval)
    }
}

groupbyhandler = function(e, collector, basedir, input, formulaInputs, update, pipe = FALSE, nseval = FALSE, ...) {
    nms = names(e)
    add = which(nms=="add")
    if(length(add)) {
        getInputs(e[[add]], collector = collector,
                  basedir = basedir, input = input, formulaInputs = formulaInputs,
                  update = update, pipe = pipe, nseval = nseval, ...)
        e = e[-add]
    }

    nseafterfirst(e, collector = collector, basedir = basedir, input = input,
                  formulaInputs = formulaInputs, update = update,
                  pipe = pipe, nseval = nseval, ...)


}

counthandler = function(e, collector, basedir, input, formulaInputs, update, pipe = FALSE, nseval = FALSE, ...) {
    nms = names(e)
    srt = which(nms == "sort")
    if(length(srt)) {
        getInputs(e[[srt]], collector = collector,
                  basedir = basedir, input = input, formulaInputs = formulaInputs,
                  update = update, pipe = pipe, nseval = nseval, ...)
        e = e[-srt]
    }
 
    nseafterfirst(e, collector = collector, basedir = basedir, input = input,
                  formulaInputs = formulaInputs, update = update,
                  pipe = pipe, nseval = nseval, ...)
}

##filter(),mutate(),mutate_each(),transmute(),rename(),slice(),summarise(),summarize(),summarise_each(),arrange(),select(),group_by(),group_indices(),data_frame(),distinct(),do(),funs(),count()

colonshandler = function(e, collector, basedir, input, formulaInputs, update, pipe = FALSE, nseval = FALSE, ...) {
    collector$library( as.character(e[[2]]))
    collector$call(as.character(e[[1]]))
    collector$call(as.character(e[[3]]))
}

spreadhandler = function(e, collector, basedir, input, formulaInputs, update, pipe = FALSE, nseval = FALSE, ...) {
    ## second and third args are nseval, rest are not.
    collector$call("spread")
    if(!pipe)
        getInputs(e[[2]], collector = collector, basedir = basedir, input = input, update = update, pipe = pipe, nseval = FALSE, ...)

    lapply(e[3:4], getInputs, collector = collector, basedir = basedir, input = input, update = update, pipe = pipe, nseval = TRUE, ...)
    if(length(e) >=5)
        lapply(e[5:length(e)], getInputs, collector = collector, basedir = basedir, input = input, update = update, pipe = pipe, nseval = FALSE, ...)
       
}

forhandler = function(e, collector, basedir, input, formulaInputs, update, pipe = FALSE, nseval = FALSE, ...) {
    collector$call(as.character(e[[1]]))
    collector$vars(as.character(e[[2]]), input=FALSE)
    getInputs(e[[3]], collector = collector, basedir = basedir, input=TRUE, update = update, pipe = pipe, nseval=FALSE, ...)
    getInputs(e[[4]], collector = collector, basedir = basedir, input=input, update = update, pipe = pipe, nseval=FALSE, ...)
}

ifforcomp = function(e, collector, basedir, input, formulaInputs, update, pipe = FALSE, nseval = FALSE, ...) {
    collector$calls("if")
    getInputs(e[[2]], collector = collector, basedir = basedir, input = input, update = update, pipe = pipe, nseval=FALSE, ...)
    
    innerres = getInputs(e[[3]], inputCollector(functionHandlers = collector$functionHandlers))
    collector$vars(innerres@inputs, input=TRUE)
    collector$library(innerres@libraries)
    collector$string(innerres@strings, basedir = basedir, filep=FALSE)
    collector$string(innerres@files, basedir=basedir, filep=TRUE)
    collector$calls(innerres@functions)
    

}

defaultFuncHandlers = list(
    library = libreqhandler,
    require = libreqhandler,
    requireNamespace = libreqhandler,
    rm = rmhandler,
    "$" = dollarhandler,
    "@" = dollarhandler,
    "=" = assignhandler,
    "<-" = assignhandler,
    "<<-" = assignhandler,
    "function" = funchandler,
    "~" = formulahandler,
    "assign" = assignfunhandler,
    aes = fullnsehandler,
    subset = nseafterfirst,
    transform = nseafterfirst, 
    filter = filterhandler,
    mutate = nseafterfirst,
    mutate_each = nsehandlerfactory(2),
    transmute = nseafterfirst,
    rename = nseafterfirst,
    slice =  nseafterfirst,
    summarise =  nseafterfirst,
    summarize =  nseafterfirst,
    summarise_each = nsehandlerfactory(2),
    arrange =  nseafterfirst,
    select =  nseafterfirst,
    group_by = groupbyhandler,
    group_indices =  nseafterfirst,
    data_frame = fullnsehandler,
    distinct =  nseafterfirst,
    do = nseafterfirst,
    funs = fullnsehandler,
    count = counthandler,
    tally = counthandler,
    arrange = nseafterfirst,
    spread = spreadhandler,
    unnest = nseafterfirst,
    with = nseafterfirst,
    "::" = colonshandler,
    ":::" = colonshandler,
    "%>%" = pipehandler,
    "for" = forhandler,
    "_assignment_" = assignhandler,
    "_default_" = defhandler
    
    
    )

isAssignment = function(e) class(e) %in% c("=", "<-") || (is.call(e) && is.symbol(e[[1]]) && as.character(e[[1]]) == "<<-")


