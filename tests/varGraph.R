library(CodeDepends)
sc = readScript("varGraphEg2.R")
gfs = getFunctionGlobals(sc)
# Or use getGlobals from CodeAnalysis for improved version.
gfs2 = getFunctionGlobals(sc, getGlobals = CodeAnalysis::getGlobals)          
gg = getVariableGraph("r", sc, functionGlobals = gfs)




if(FALSE) {
sc2 = readScript("~/Book/CodeReview/LaurenOrig/TOY.R")
gfs2 = getFunctionGlobals(sc2)
# For TOY2.R, the functions are in functions.R
                #getFunctionDefs("~/Book/CodeReview/LaurenOrig/functions.R")
# gfs2 = getFunctionGlobals("~/Book/CodeReview/LaurenOrig/functions.R")
# No need for this.
# e = new.env(); source("~/Book/CodeReview/LaurenOrig/functions.R", e); e = as.list(e); e = e[sapply(e, is.function)]
#gfs2 = lapply(e, function(x) unique(getGlobals(x)$variables))
#gfs2 = gfs2[sapply(gfs2, length) > 0]
gg2 = getVariableGraph("Vstates", sc2, functionGlobals = gfs2)
}
