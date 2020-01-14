###################

# To deal with functions and global variables, we need to know
# where the function is defined. We can see if it exists and use that definition, but this is potentially problematic.
# If we have the script,  we can look for the definition there. Again, we have to be careful about conditional
# definitions.  Also, if we have a source() in the script, we can follow that.
#
# Once we have the function, we can use getGlobals() to find its global variables.
# Then we add those to variables on which the current output from the call depends.
# Should we do this here just for the graph, in getVariableDepends, or in  getInputs
# and add the variables to the inputs. We can merge the globals with the regular inputs
# but make want to keep them separate.

# See getFunctionGlobals() for a script and related functions in getFunctions.R
#
#
#


getVariableGraph = getVarDependsGraph =
function(var, sc, asGraph = TRUE, globals = TRUE,
         idx = getVariableDepends(var, sc, asIndex = TRUE, functionGlobals = functionGlobals),
         functionGlobals = getFunctionGlobals(sc))
{
    df = createVarGraph(sc[idx], sc, globals, functionGlobals)
    if(asGraph)
        igraph::graph_from_data_frame(df)
    else
        df
}

createVarGraph =
function(cmds, script = NULL, globals = TRUE, functionGlobals = getFunctionGlobals(cmds))
{
    info = lapply(cmds, getInputs)
    do.call(rbind, lapply(info, inputOutputEdges, globals, functionGlobals))
}

inputOutputEdges =
function(info, globals, functionGlobals = NULL)
{
    ins = getGlobalInputs(info, functionGlobals) # unique(info@inputs)
    if(FALSE && length(functionGlobals)) {
#        browser()
        i = match(names(info@functions), names(functionGlobals), 0)
        if(any(i > 0))
           ins = c(ins, unlist(functionGlobals[i]))
    }
    
    outs = unique(c(info@outputs, info@updates))
#    if(length(outs) > 1)
#        warning("more than one output for ", paste(deparse(info@code), sep = " "))
    expand.grid(input = ins, output = outs, stringsAsFactors = FALSE)
}

