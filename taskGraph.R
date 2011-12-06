tgraph =
function(script, info = as(script, "ScriptInfo"))
{
  deps = lapply(seq(along = info),
          function(i) {
              if(i == 1)
                 return(integer())
              
              setdiff(getDependsThread(info[[i]]@outputs, info[1:i]), i)
          })


  sofar = integer()
  levels = list()
  cur = integer()
  for(i in seq(along = deps)) {
     d = deps[[i]]
     if(length(d) == 0) {
       cur = c(cur, i)
       next
     }

     if(any(d %in% cur)) {
       levels[[ length(levels) + 1]] =  cur
       cur = i
       next
     }
  }
  levels                               
}



