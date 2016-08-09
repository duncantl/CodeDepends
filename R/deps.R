
splitRedefinitions =
  #
  # Takes a script and its information and 
  # breaks it into disjoint contiguous groups
  # that represent different definitions/assignments to the specified
  # variable.
  # These redefinitions might be as simple as
  #  colnames(x) = "foo"
  # but it allows us to avoid 
function(var, info)
{
  w = isDefinedVar(var, info)
  pos = which(w)
  start = c(1, pos + 1)[- (sum(w) + 1) ]
  xx = cbind(start, pos )
  lapply(seq(length = nrow(xx)),
          function(j) {
             info[seq(xx[j,1], xx[j,2])]
          })
}

isDefinedVar =
function(var, info)
  sapply(info, function(x) any(var %in% getVariables(x)))



findLastDef = function(var, info) {

  w = isDefinedVar(var, info)
  if(!any(w))
    return(integer())

  last = max(which(w))

  last
}



setGeneric("getDependsThread", function(var, info, reverse = TRUE)
    standardGeneric("getDependsThread"))

setMethod("getDependsThread", "character", function(var, info, reverse) {
              expr = parse(text=var)
              if(length(expr) == 1 && is(expr[[1]], "name")) {
                  ## This is a variable name
                  position = findLastDef(var, info)
                  
              } else {
                  ## it's a code expression
                  matches = sapply(info, function(x) identical(expr, x@code) || identical(expr[[1]], x@code))
                  if(sum(matches) >= 1)
                      position = max(which(matches))
                  else
                      stop("string passed to var seems to be an expression that does not appear in info")
              }
              ## Delegate to numeric method
              getDependsThread(var = position, info = info, reverse = reverse)
          })

setMethod("getDependsThread", "numeric", function(var, info, reverse) {
              
              .getDependsThread(position = var, info = info, reverse = reverse)

          })


setMethod("getDependsThread", "name", function(var, info, reverse) {
    ## Delegate to character method
    getDependsThread(as.character(var), info = info, reverse = reverse)
    })

.getDependsThread =
  # This is an iterative version that walks back down the
  # ScriptNodeInfo  list picking out the ones that are needed to run.
function(var, info, position = findLastDef(var, info), reverse = TRUE)
{
    ans = position
    vars = info[[position]]@inputs
    while(TRUE) {

    sub = info[1:(position-1)]    
    w = isDefinedVar(vars, sub)
    if(!any(w))
       break

    position = max(which(w))
    ans = c(ans, position)
    if(position == 1)
      break
      # Do any outputs from this block take care of variables in
      # the ones we are looking for.
    vars = vars[!(vars %in% getVariables(sub[[position]]))]
    vars = c(vars, sub[[position]]@inputs)

  }

  if(reverse) rev(ans) else ans
}



