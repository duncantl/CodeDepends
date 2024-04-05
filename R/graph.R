
## some "var" names are not allowed as node labels
## I know that | and || are not allowed, I guessed about some
## other things that might also cause problems. Seems to work now ~GB
disallowed = c("|" = "vectorized or",
    "||" = "binary or",
    "&&" = "binary and",
    "&" = "vectorized and",
    "[" = "square bracket",
    "(" = "parenthesis",
    "==" = "equal op",
    "<-" = "left assign")
makeNodeLabs = function(vars) {
    mtch = match(vars, names(disallowed))
    vars[!is.na(mtch)] = disallowed[mtch[!is.na(mtch)]]
    vars
}


getDependsOn =
  #
  # Find what variables depend on this one variable.
  #
  # This leaves out tasks that have no output variables.
  #
function(var, info, vars = character())
{
    # search through all the code blocks and see which
    # one consume this variable.
  w = sapply(info, function(x) var %in% x@inputs)
 
     # now go through all those
  ans = unique(unlist(c(lapply(info[w], getVariables))))
  if(length(vars)) {
    list(edges = match(ans, vars))
  } else
    ans
}


makeVariableGraph =
  #
  # What about duplicates, i.e. redefinitions of variables.
  #
function(doc, frags = readScript(doc), info = getInputs(frags), vars = getVariables(info, inputs = free), free = TRUE)
{
  if(!requireNamespace("graph"))
      stop("Cannot make task graph without the graph package (available from the Bioconductor project).")
  vars = unique(vars)

  edges = lapply(vars, getDependsOn, info = info, vars = vars)

  ## Fix var names to be allowable node labels ~GB
  vars = makeNodeLabs(vars)
  names(edges) = vars
  new("graphNEL", nodes = vars, edgeL = edges, edgemode = "directed")
}


###########################################


makeScriptNodeNames =
function(info)
{
  ids =sapply(seq(along = info),
                function(i) {
                   vars = getVariables(info[[i]])
                   if(length(vars))
                      paste(vars, collapse = ", ")
                   else
                      paste("Task", i)
           })

  dup = duplicated(ids)
  if(any(dup)) {
       #XXX do this properly, i.e. name them 1, 2, 3.
    ids[dup] = paste("Task", which(dup))
  }
  ids
}

makeTaskGraph =
function(doc, frags = readScript(doc), info = as(frags, "ScriptInfo"))
{
  if(!requireNamespace("graph"))
      stop("Cannot make task graph without the graph package (available from the Bioconductor project).")
  nodeIds = if(length(names(info)) && all(!is.na(names(info))))
                 names(info)
            else
                 makeScriptNodeNames(info)
  
  names(info) = nodeIds
  edges = lapply(info, function(x) {
                  list(edges =  getPropagateChanges(getVariables(x), info, index = TRUE))
                })

  new("graphNEL", nodes = nodeIds, edgeL = edges, edgemode = "directed")
}
                    

#######################################

getTimelines =
  #
  # We want to be able to determine when a variable starts
  # and when it ends, either when it can be removed or
  # when it is reassigned.
  # 
  # We also may want to know at what time steps the variables are used
  # and when they are redefined.
  #
function(doc, info = getInputs(doc), vars = getVariables(info))
{
    # loop over each block/time step
  ans = lapply(seq(along = info),
                function(i) {
                       # then each variable
                    rest = info[seq(i, length(info))]
                    vars = getVariables(info[[i]])
                    if(length(vars) == 0)
                      return(data.frame(var = character(), end = integer(), step = integer()))
                    when = sapply(vars,  findWhenUnneeded, info = rest, end = length(info), redefined = TRUE)
                    data.frame(var = vars, end = when, step = rep(i, length(when)))
                })

  ans = do.call("rbind", ans)
  rownames = vars
  structure(ans, class = c("VariableTimeline", "data.frame"))
}


plot.VariableTimeline =
function(x, ...)
{
  plot(1, type = "n", xlim = c(1, max(x$end)), ylim = c(1, nrow(x)), axes = FALSE, mar = c(5, 10, 4, 2))
  axis(1)
  axis(2, at = 1:nrow(x), labels = x$var, las = 2, cex = .5)
  sapply(seq(length = nrow(x)),
          function(row)
            lines(c(x$step[row], x$end[row]), c(row, row)))
  TRUE
}


getDetailedTimelines =
function(doc, info = getInputs(doc,...), vars = getVariables(info, functions = functions), functions = TRUE, ...)
{
  ans = lapply(unique(vars), getDetailedTimeline, info = info, functions = functions)
  ans = do.call("rbind", ans)
  ans$var = factor(ans$var, levels = unique(ans$var))
  rownames(ans) = NULL
  structure(ans, class = c("DetailedVariableTimeline", "data.frame"), range = c(1, length(info)),  scriptName = if(!missing(doc)) doc@location else as.character(NA))
}

getDetailedTimeline =
#
# Want to be able to determine when the variable could
# be garbage collected and is reassigned and identify the
# gap.
function(var, info, functions = TRUE)
{
  used = sapply(info, function(x) var %in% x@inputs)
  defined = sapply(info, function(x) var %in% getVariables(x, functions = functions))
  data.frame(step = 1:length(info), used = used, defined = defined, var = rep(var, length(defined)), stringsAsFactors = FALSE)
}

if(FALSE) {
f = system.file("samples", "results-multi.R", package = "CodeDepends")
dtm = getDetailedTimelines(, getInputs(readScript(f)))
plot(dtm)
table(dtm$var)
}

setAs("DetailedVariableTimeline", "matrix",  #??? Was this really DetailedVariableTime
      function(from) {
         matrix()
      })

plot.DetailedVariableTimeline =
function(x, var.srt = 0, var.mar = round(max(4, .5*max(nchar(levels(x$var))))), var.cex = 1,
           main = attr(x, "scriptName"), ...)
{
  old = par(no.readonly = TRUE)
  on.exit(par(old))
  par(mar = c(5, var.mar, 4, 1) + .1)
  numVars = length(levels(x$var))
  plot(1, type = "n", xlim = attr(x, "range"), ylim = c(1, numVars),
         yaxt = "n",  xlab = "Step", ylab = "", main = main, ...)

  text(par("usr")[1] - 0.25, 1:numVars, srt = var.srt, adj = 1,
          labels = levels(x$var), xpd = TRUE, cex = var.cex)

  
  x$varNum = as.integer(x$var)

  by(x, x$var, function(x) {
                  i = x$varNum[1]
                  start = c(min(which(x$defined)), if(any(x$used)) max(which(x$used)) else min(which(x$defined)))
                  lines(c(1, start[1]), c(i, i), col = "lightgray", lty = 3)                  
                  lines(start, c(i, i), col = "lightgray")
                  if(any(x$used))
                    points(which(x$used), rep(i, sum(x$used)), pch = 21, col = "red")
                  if(any(x$defined))
                    points(which(x$defined), rep(i, sum(x$defined)), pch = 23, cex = 1.3, col = "green")
                })

  legend(1, numVars, pch = c(21, 23), legend = c("used", "(re)defined"), col = c("red", "green"))
  TRUE
}
