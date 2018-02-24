findSideEffects =
  #
  #  could be a call of the form cb$pause()
  #
function(call, collector)
{
    # generally need to match.call but file has to be matched exactly in cat.
  fn = call[[1]]
  if(isNSVar(fn)) 
     fn = fn[[3]]
  if(is.call(fn) || length(as.character(fn)) > 1) ## we're in the cb$pause() realm, don't do anything for now
      return(NULL)
    
  var = NULL
  if(as.character(fn) == "cat" && "file" %in% names(call)) {
        # might be a literal or a call.
        # If this was something like   connections[[ "foo" ]]
        # we wouldn't recognize that at present.  ????
      var = call[["file"]]
  } else if(as.character(fn) == "close") {
      var = fn
  }

  if(is.name(var))
    collector$sideEffects(as.character(var))  

  NULL
}

