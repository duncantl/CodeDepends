findWhenVarUnneeded =
function(v, info, end = NA, redefined = FALSE) {
  i = sapply(info, function(x) v %in% x@inputs)
  ans = if(!any(i))
    end
  else
    which.max(cumsum(i))
  if(redefined) {
    tmp = sapply(info, function(x) v %in% getVariables(x))
    if(sum(tmp) > 1) 
      ans = min(ans, which(tmp)[2], na.rm = TRUE)
  }
  
  ans
}  


# This finds when a variable can be removed, i.e. when it is no longer needed.
findWhenUnneeded =
function(var, frags, info = lapply(frags, getInputs), simplify, index = TRUE, end = NA,
          redefined = FALSE)
{
  i = sapply(var, findWhenVarUnneeded, info = info, end = end, redefined = redefined)
  if(index)
    structure(i, names = var)
  else
    structure(frags[i], names = var)
}


addRemoveIntermediates =
  #
  # We have to find if the script itself removes variables
  #
  #
function(doc, frags = readScript(doc), info = getInputs(frags),
          vars = getVariables(info))
{
  end = sapply(vars, findWhenUnneeded, info = info)

    # Group the end points and remove all the variables there.
  by(data.frame(var = vars, end = end), end,
       function(x) {
          i = x[1,"end"]
          if(is.na(i))
            return()
          f = frags[[ i ]]
          k = call("rm")
          k[seq(2, length = nrow(x)) ] = lapply(as.character(x[, "var"]), as.name)

          if(class(f) == "{")
             f[ length(f) + 1 ] = k
          else {
            tmp = expression()
            tmp[[1]] = f
            tmp[[2]] = k
            f = tmp
          }
          frags[[i]] <<- f
       })
  
  frags
}
