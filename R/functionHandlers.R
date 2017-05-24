

libreqhandler = function(e, collector, basedir, input, formulaInputs, update, pipe = FALSE,  ...)  collector$library(as.character(e[[2]]))

rmhandler = function(e, collector, basedir, input, formulaInputs, update, pipe = FALSE, nseval = FALSE, ...) collector$removes(sapply(e[-1], as.character))

dollarhandler = function(e, collector, basedir, input, formulaInputs, update, pipe = FALSE, nseval = FALSE, ...) {
    


    ##need to handle cases like a$b$c, which translate to `$`(a$b, c), correctly.
    ## Only a is a real variable here! Identified based on MathiasHinz
    ## https://github.com/duncantl/CodeDepends/issues/4
    ## make sure that @ or $ is listed in the called functions
    if(is(e[[1]], "name"))
        collector$call(as.character(e[[1]]))
    if(is(e[[2]], "name"))
        collector$vars(as.character(e[[2]]), input = input)
    else
        getInputs(e[[2]], collector = collector, basedir = basedir, input = input, formulaInputs = formulaInputs,
                  update = update, pipe = pipe, nseval = nseval, ...)
}

isbrack = function(x) asVarName(x) %in% c("[", "[[")

assignhandler = function(e, collector, basedir, input, formulaInputs, update, pipe = FALSE, nseval = FALSE, ...) {


    ## Need to handle updates, e.g.
    ##   foo(x) = 1
    ##   x[["y"]] = 2
    ##   x [ x > 0 ] = 2 * y
    ##   x = x + 5


    ## Do the left hand side first.

    ## I dont' think we CAN do the LHS first. We need to know if variable is an
    ## input to the expression so we know if it's an output or an update!! ~GB
    
    ##if it is a simple name, then it is an output,
    ## but otherwise it is a call and may have more inputs.

    ## asVarName returns, e.g., "x" for x[!y], and bar from foo(bar) so it's always the right thing...
    lhs = e[[2]]
    outvar = asVarName(lhs)
    if(!is.name(e[[2]])) {
                
    
        fname = asVarName(lhs[[1]])
   
      
  #  browser()        
        ## if this is a x$foo <- val or x[["foo"]] = val or x[i, j] <- val
        ## make certain to add the variable being updated as an input.
        ## It will also be an output. 


        ## anything that is being updated must be an input!  This
        ## is a behavior change but it seems necessary for the
        ## dependency stuff to function correctly.
        collector$vars(outvar, input = TRUE)
        ## this pattern holds for both x[foo] = 5 and foo(x) = 5. x is e[[2]][[2]] in both cases.

        ##   collector$update(outvar)

        if(fname == "$")
            numtoskip = 3
        else
            numtoskip = 2
        
        if(length(lhs) > numtoskip) {
            lapply(numtoskip:length(lhs), function(i, ...) getInputs(lhs[[i]], ...), collector = collector, basedir = basedir, input = TRUE, formulaInputs = formulaInputs, ..., pipe = pipe, nseval = nseval, update=FALSE)
        }
        collector$call(paste(fname, "<-", sep = ""))
        ## needs to modify getInputs state. a bit of a sharp edge for the refactor ~GB
        update = TRUE
        
    

    
        ## if(as.character(e[[2]][[1]]) != "$")
        ##     lapply(as.list(e[[2]][-c(1,2)]), getInputs, collector, basedir = basedir, input = FALSE, formulaInputs = formulaInputs, ..., update = update, pipe= pipe, nseval = nseval)
    } else {
        ## collector$set(asVarName(e[[2]]))
    }

    ## Do the right hand side
    lapply(3:length(e), function(i, ...) getInputs(e[[i]], ...), collector, basedir = basedir, input = TRUE, formulaInputs = formulaInputs, update = FALSE, pipe = pipe, nseval = nseval)

    ## if(is.name(e[[2]])) 
    ##     collector$set(asVarName(e[[2]]))
    ## else {
    ##     if(is.call(e[[2]])) {
    ##         ##XXX will get foo in foo(x)
    ##         if(!update)
    ##             collector$set(asVarName(e[[2]][[2]]))
    ##         if(as.character(e[[2]][[1]]) != "$")                   
    ##             lapply(e[[2]][-c(1,2)], getInputs, collector, basedir = basedir, input = input, formulaInputs = formulaInputs, ..., update = update, pipe = pipe, nseval = nseval)
    ##     } else {
    ##         collector$set(asVarName(e[[2]][[2]]))
    ##     }
    ## }
    if(outvar %in% collector$results()@inputs)
        update = TRUE

    if(update)
        collector$update(outvar)
    else
        collector$set(outvar)
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
        if(is.character(e[[2]]))
           collector$set(e[[2]]) ##variable
        else
           collector$set(structure(as.character(NA), names = deparse(e[[2]])))
        getInputs(e[[3]], collector = collector, basedir = basedir, input = TRUE,
                  formulaInputs = formulaInputs, update = update, pipe = pipe,
                  nseval = nseval, ... )
    }

}

fullnsehandler = function(e, collector, basedir, input, formulaInputs, update, pipe = FALSE, nseval = FALSE, ...) {
    collector$calls(as.character(e[[1]]))
    lapply(as.list(e[-1]), getInputs, collector = collector, basedir = basedir,
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

##filter(),mutate(),mutate_each(),transmute(),rename(),slice(),summarise(),summarize_(),summarise_each(),arrange(),select(),group_by(),group_indices(),data_frame(),distinct(),do(),funs(),count()

colonshandler = function(e, collector, basedir, input, formulaInputs, update, pipe = FALSE, nseval = FALSE, ...) {
    collector$library( as.character(e[[2]]))
    collector$call(as.character(e[[1]]))
    collector$call(as.character(e[[3]]))
}

spreadhandler = function(e, collector, basedir, input, formulaInputs, update, pipe = FALSE, nseval = FALSE, ...) {
    ## second and third args are nseval, rest are not.
    collector$call("spread")
    if(!pipe)
        getInputs(e[[2]], collector = collector, basedir = basedir, input = input, formulaInputs = formulaInputs,  update = update, pipe = pipe, nseval = FALSE, ...)

    lapply(e[3:4], getInputs, collector = collector, basedir = basedir, input = input, formulaInputs = formulaInputs,  update = update, pipe = pipe, nseval = TRUE, ...)
    if(length(e) >=5)
        lapply(e[5:length(e)], getInputs, collector = collector, basedir = basedir, input = input, formulaInputs = formulaInputs, update = update, pipe = pipe, nseval = FALSE, ...)
       
}

forhandler = function(e, collector, basedir, input, formulaInputs, update, pipe = FALSE, nseval = FALSE, ...) {
    collector$call(as.character(e[[1]]))
    collector$vars(as.character(e[[2]]), input=FALSE)
    getInputs(e[[3]], collector = collector, basedir = basedir, input=TRUE, formulaInputs = formulaInputs,  update = update, pipe = pipe, nseval=FALSE, ...)
    getInputs(e[[4]], collector = collector, basedir = basedir, input=input, formulaInputs = formulaInputs, update = update, pipe = pipe, nseval=FALSE, ...)
}

ifforcomp = function(e, collector, basedir, input, formulaInputs, update, pipe = FALSE, nseval = FALSE, ...) {
    collector$calls("if")
    getInputs(e[[2]], collector = collector, basedir = basedir, input = input,  formulaInputs = formulaInputs, update = update, pipe = pipe, nseval=FALSE, ...)
    
    innerres = getInputs(e[[3]], inputCollector(functionHandlers = collector$functionHandlers), basedir = basedir,  formulaInputs = formulaInputs)
    collector$vars(innerres@inputs, input=TRUE)
    collector$library(innerres@libraries)
    collector$string(innerres@strings, basedir = basedir, filep=FALSE)
    collector$string(innerres@files, basedir=basedir, filep=TRUE)
    collector$calls(innerres@functions)
    

}

dataformals = names(formals(data))[-1] # first formal is ..., those are nsevaluated

## XXX This grabs the symbols for the datasets being laoded, and counts them as
## nseval. I'm not sure this is valuable to do and may be actively misleading.
## Could easily make it not do that, but I'll leave it as is for now.
datahandler = function(e, collector, basedir, input, formulaInputs, update, pipe = FALSE, nseval = FALSE, ...) {
    collector$calls(as.character(e[[1]]))
   
    for(i in 2:length(e)) {
        getInputs(e[[i]], collector = collector, basedir = basedir,
                  input = TRUE,  formulaInputs = formulaInputs, update = update, pipe = pipe,
                  ## it's nseval IFF it's eaten by the dots, ie not in dataformals
                  nseval = !(is.null(names(e)) || names(e)[i] %in% dataformals),
                  ...)
    }

    ## protect against data(package="bla") case wehre nothing is actually loaded
    if(!all(names(e)[-1] %in% dataformals)) {
        ## do the datacall in a temp environment so we can grab what it loads
        myenv = new.env()
        e2 = e
        e2$envir = myenv
        ## returns value all specified datasets, even ones that don't exist!!!
        res = suppressWarnings(eval(e2))
        ## we check in the envir to see which ones were actually real.
        collector$vars(ls(myenv), input= FALSE)
    }
}

applyhandlerfactory = function(funpos, funargname = "FUN", inmap = FALSE) {
    force(funpos)
    applyhandler = function(e, collector, basedir, input, formulaInputs, update, pipe = FALSE, nseval=FALSE, ...) {
        if(pipe)
            funpos = funpos - 1
        
        collector$calls(as.character(e[[1]]))
        hasnamedfun = "FUN" %in% names(e)
        ## protect against hadley's weird formula anonymous function thing
        ##
        ## XXX There are corner cases where this is probably going to
        ## get it wrong.
        if(inmap)
            formulaInputs = FALSE
        
        lapply(2:length(e), function(i) {
            if((hasnamedfun && names(e)[i] == "FUN") || (!hasnamedfun && i == funpos)) {
                if(is.name(e[[i]])){
                    collector$calls(asVarName(e[[i]]))
                } else {
                    getInputs(e[[i]], collector = collector, basedir = basedir, formulaInputs = formulaInputs, update = update, pipe = pipe, nseval = nseval, ...)
                }
            } else {
                getInputs(e[[i]], collector = collector, basedir = basedir, formulaInputs = formulaInputs, update = update, pipe = pipe, nseval = nseval, ...)
            }
        })
    }
    applyhandler
}
        
                          

  #          collector
    

summarize_handlerfactory = function(funspos = 3) {

    ## we're going to allow funspos to be a vector but assume it is contiguous, ie of the form n:m with m>n

    ret = function(e, collector, basedir, input, formulaInputs, update, pipe = FALSE, nseval=FALSE, ...) {
        newcol = do.call(inputCollector, collector$collectorSettings())
        collector$calls(asVarName(e[[1]]))
        if(pipe)  {
            funspos = funspos-1
        }
        inds = funspos:length(e)
        getInputs(e[[2]], collector = collector, basedir = basedir,
                  input = input, formulaInputs = formulaInputs,
                  update =update,  pipe = FALSE, nseval = FALSE)
        
        if( min(funspos) > 3){
            beffunspos = 3:(min(funspos)-1)
            lapply(as.list(e[beffunspos]), getInputs, collector = collector, basedir = basedir, input = input, formulaInputs = formulaInputs, update = update, pipe = FALSE, nseval = FALSE, ...)
        }
        lapply(funspos, function (i) .funhandler(e[[i]], collector = collector, basedir = basedir, input = input, formulaInputs = formulaInputs, update = FALSE, pipe = FALSE, nseval = FALSE, ..., iscalled=TRUE))
        
        
        if(length(e) > max(funspos))
            lapply(as.list(e[(max(funspos)+1):length(e)]), getInputs, collector = collector, basedir = basedir, input = input, formulaInputs = formulaInputs, update = FALSE, pipe = FALSE, nseval = FALSE, ..., iscalled=TRUE)
    }
    ret
}

.funhandler = function(e, collector, basedir, input, formulaInputs, update, pipe, nseval, ..., iscalled = TRUE) {
    if(is.call(e) && asVarName(e[[1]]) == "funs") {
        collector$calls("funs")
        if(length(e) > 1) {
            lapply(2:length(e), function(i, ...) .funhandler(e[[i]], ...),
                   collector = collector, basedir = basedir, input = input, formulaInputs = formulaInputs,
                   update = update, pipe = FALSE, nseval = FALSE, iscalled = iscalled)
            return()
        }
    }
    if(is.name(e) || is.character(e)) {
        if(iscalled)
            collector$calls(asVarName(e))
        else
            collector$vars(asVarName(e), input=TRUE)
    }else {
        inres = getInputs(e, collector = do.call(inputCollector, collector$collectorSettings()),
                          basedir = basedir, input = input, formulaInputs = formulaInputs, update = update,
                          pipe = pipe, nseval = nseval)
        collector$calls(names(inres@functions))
        collector$vars(setdiff(inres@inputs, "."), input=TRUE)
        collector$string(inres@strings, filep=FALSE)
        collector$nseval(inres@nsevalVars)
        collector$string(inres@files, filep= TRUE)
    }
    
        
}

funshandler = function(e, collector, basedir, input, formulaInputs, update, pipe = FALSE, nseval=FALSE, ...) {
    collector$calls(e[[1]])
    if(length(e) > 2) {
        lapply(2:length(e), function(i) .funhandler(e[[i]], collector = collector, basedir = basedir, input = input, formulaInputs = formulaInputs, update = update, pipe = FALSE, nseval = nseval, ...))
    }
}

    
## ## ugh, dplyr just keeps making this harder and harder
## ## funs(mean, "mean", mean(., blahblabla))
## funshandler = function(e, collector, basedir, input, formulaInputs, update, pipe = FALSE, nseval=FALSE, ...) {
##     collector$calls("funs")



## }

## Add test case to inst/samples/funchandlers.R whenever ANY new entries are
## added to defaultFuncHandlers, even if they reuse an existing handler (they
## may not always).

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
    vars = fullnsehandler,
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
    ##   funs = funshandler, #fullnsehandler,
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
    data = datahandler,
    apply = applyhandlerfactory(funpos = 4), #apply, x, MARGIN, FUN
    lapply = applyhandlerfactory(funpos = 3), #lapply, x, FUN
    sapply = applyhandlerfactory(funpos = 3), #sapply, x, FUN
    mapply = applyhandlerfactory(funpos = 2), #mapply, FUN, ...
    tapply = applyhandlerfactory(funpos = 4), #tapply, x, INDEX, FUN
    ## I should really probably allow dynamic/pattern-based matching
    ## ... but I don't right now
    map = applyhandlerfactory(funpos = 3, funargname = ".f", inmap = TRUE),
    map_dbl = applyhandlerfactory(funpos = 3, funargname = ".f", inmap = TRUE),
    map_chr = applyhandlerfactory(funpos = 3, funargname = ".f", inmap = TRUE),
    map_int = applyhandlerfactory(funpos = 3, funargname = ".f", inmap = TRUE),
    map_lgl = applyhandlerfactory(funpos = 3, funargname = ".f", inmap = TRUE),
    map_df = applyhandlerfactory(funpos = 3, funargname = ".f", inmap = TRUE),
    map2_dbl = applyhandlerfactory(funpos = 4, funargname = ".f", inmap = TRUE),
    map2_chr = applyhandlerfactory(funpos = 4, funargname = ".f", inmap = TRUE),
    map2_int = applyhandlerfactory(funpos = 4, funargname = ".f", inmap = TRUE),
    map2_lgl = applyhandlerfactory(funpos = 4, funargname = ".f", inmap = TRUE),
    map2_df = applyhandlerfactory(funpos = 4, funargname = ".f", inmap = TRUE),
    map_if = applyhandlerfactory(funpos = 4, funargname = ".f", inmap = TRUE),
    map_at = applyhandlerfactory(funpos = 4, funargname = ".f", inmap = TRUE),
    pmap = applyhandlerfactory(funpos = 3, funargname = ".f", inmap = TRUE),
    pmap_dbl = applyhandlerfactory(funpos = 3, funargname = ".f", inmap = TRUE),
    pmap_chr = applyhandlerfactory(funpos = 3, funargname = ".f", inmap = TRUE),
    pmap_int = applyhandlerfactory(funpos = 3, funargname = ".f", inmap = TRUE),
    pmap_lgl = applyhandlerfactory(funpos = 3, funargname = ".f", inmap = TRUE),
    pmap_df = applyhandlerfactory(funpos = 3, funargname = ".f", inmap = TRUE),
    summarize_all = summarize_handlerfactory(3), #summarize_all, .tbl, .funs
    mutate_all = summarize_handlerfactory(3), #mutate_all, .tbl, .funs
    summarize_at = summarize_handlerfactory(4), #summarize_at, .tbl, .cols, .funs
    mutate_at = summarize_handlerfactory(4), #mutate_at, .tbl, .cols, .funs
    summarize_if = summarize_handlerfactory(3:4), #summarize_if, tbl, .predicate, .funs
    mutate_if = summarize_handlerfactory(3:4), #mutate_if, tbl, .predicate, .funs 
    vars = fullnsehandler, 
    "_assignment_" = assignhandler,
    "_default_" = defhandler
    
    
    )

isAssignment = function(e) class(e) %in% c("=", "<-") || (is.call(e) && is.symbol(e[[1]]) && as.character(e[[1]]) == "<<-")


