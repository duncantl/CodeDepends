findSideEffects =
function(call, collector)
{

    # generally need to match.call but file has to be matched exactly in cat.
  
  if(as.character(call[[1]]) == "cat" && "file" %in% names(call)) {
        # might be a literal or a call.
        # If this was something like   connections[[ "foo" ]]
        # we wouldn't recognize that at present.  ????
      var = call[["file"]]
      if(is.name(var))
        collector$sideEffects(as.character(var))
  }

  NULL
}

