
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


getDependsThread =
  # This is an iterative version that walks back down the
  # ScriptNodeInfo  list picking out the ones that are needed to run.
function(var, info, reverse = TRUE)
{
  w = isDefinedVar(var, info)
  if(!any(w))
    return(integer())

  last = max(which(w))

  if(last == 1)
    return(1L)

  ans = last
  vars = info[[last]]@inputs
  while(TRUE) {

    sub = info[1:(last-1)]    
    w = isDefinedVar(vars, sub)
    if(!any(w))
       break

    last = max(which(w))
    ans = c(ans, last)
    if(last == 1)
      break
      # Do any outputs from this block take care of variables in
      # the ones we are looking for.
    vars = vars[!(vars %in% getVariables(sub[[last]]))]
    vars = c(vars, sub[[last]]@inputs)

  }

  if(reverse) rev(ans) else ans
}



