blocks = parse("../inst/samples/isim.R")
info = lapply(blocks, getInputs)

e = getVariableDepends("T", blocks, info)
lapply(e, eval)

# http://penguin.biostat.jhsph.edu/cpkg/6050/957b807d511ba76549c81b6e43a1c4477696/src/mcaps.R
