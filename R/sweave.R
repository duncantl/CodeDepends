getTangledFrags =
  #
  #  getTangledFrags(url("http://penguin.biostat.jhsph.edu/cpkg/49c0/90223e7b16d72240a928f69bccd72a0a164c/src/timeseries.R"))
  #
function(doc, txt = readLines(doc))
{

  pos = c(grep("^### chunk number", txt), length(txt))
  lapply(seq(along = pos[ - length(pos) ]),
          function(j) {
             parse(text = txt[ pos[j]: pos[j+1]])
          })
}
