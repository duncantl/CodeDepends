## getTangledFrags =
##   #
##   #  getTangledFrags(url("http://penguin.biostat.jhsph.edu/cpkg/49c0/90223e7b16d72240a928f69bccd72a0a164c/src/timeseries.R"))
##   #
## function(doc, txt = readLines(doc))
## {
##   pos = c(grep("(^<<[^>]>>=|^@)", txt), length(txt))
##   lapply(seq(along = pos[ - length(pos) ]),
##           function(j) {
##              parse(text = txt[ pos[j]: pos[j+1]])
##           })
## }


getTangledFrags =
  #
  #  getTangledFrags(url("http://penguin.biostat.jhsph.edu/cpkg/49c0/90223e7b16d72240a928f69bccd72a0a164c/src/timeseries.R"))
  #
function(doc, txt = readLines(doc))
{
    if(!requireNamespace("knitr"))
        stop("unable to do getTangledFrags without the knitr package")
    
  in.con = textConnection(txt)
  out.con = textConnection("bob", "w", local = TRUE)
  on.exit({close(in.con); close(out.con)})
  knitr::knit(in.con, output = out.con, tangle = TRUE, quiet = TRUE)
  code = textConnectionValue(out.con)
  parse(text = code)
}
