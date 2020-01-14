#
#
if(FALSE) {
    sc = readScript("../tests/varGraphEg2.R")

    g = getVarGraph("r", sc, functionGlobals = NULL)
    plot(g)
    # N->x  x->y x->r  y->r
    
    gfs = getFunctionGlobals(sc)          
    gg = getVarGraph("r", sc, functionGlobals = gfs)
    plot(gg)
   
    # Now N is a direct input for x *and* y
    # N ->x x ->y beta0->y pi ->y N->y y->r x->r
}

N = 10
set.seed(141212)
x = runif(N)
# If we hard-code beta0, then getVarGraph() reports correctly
# But if beta0 depends on other variables, adding the globals in inputOutputEdges doesn't work.
# needs to be in getVariableDepends()
# And then passed to getSectionDepends().

#beta0 = 2.34
mu = 3
beta0 = rnorm(1, mu)

foo = function(x)
        beta0 + pi*x + rnorm(N)

y = foo(x)


r = cor(y, x)
f = lm(y ~ x)
beta = coef(f)

