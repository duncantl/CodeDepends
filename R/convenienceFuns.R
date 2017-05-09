getLibraries =
function(info)
{
   table( unlist(lapply(info, slot, "libraries")))
}

#all.libs = table(unlist(lapply(info, getLibraries)))
