library(CodeDepends)


res7 = getInputs(quote(x <- names(y[a:5])[w]))
stopifnot(identical(res7@inputs, c("y", "a", "w")),
          identical(res7@outputs, "x"),
          identical(res7@code, quote(x <- names(y[a:5])[w])))



## regression test to ensure formulaInputs argument functions correctly
res10a = getInputs(quote(lm(x~y)))
stopifnot(identical(res10a@inputs, character()))
res10b = getInputs(quote(lm(x~y)), formulaInputs = TRUE)
stopifnot(identical(res10b@inputs, c("x", "y")))

## regression tests for passing functions directly to getInputs

f = function(a = 5, b, c = 7) {
    d = a + b + 5
    df = data.frame(a = a, b = b)
    fit =lm(b~a, data = df)
    fit
}

res11 = getInputs(f)
## one ScriptNodeInfo for the formals, then 5 for the body
stopifnot(length(res11) == 6,
          identical(res11[[5]]@inputs, "df"),
          identical(res11[[5]]@outputs, "fit"),
          identical(res11[[5]]@functions, c("lm" = FALSE, "~" = FALSE)),
          identical(res11[[5]]@nsevalVars, c("b", "a"))
          )


## does it know where functions that live in base packages come from (ie do they get FALSE)
## also does passing expressions directly to readScript work?
## We have to do this because it only tries to figure out function locality
## when it's given a script, not for individual expressions
## XXX change this for base package funs?

res12 = getInputs(readScript(txt = quote(x <- rnorm(10)+ Rcmd("This would never work!"))))

stopifnot(identical(res12[[1]]@functions, c("+" = FALSE, rnorm = FALSE, Rcmd = FALSE)))


## do functions called via the *apply statements show up in funs rather than inputs?

res13 = getInputs(quote(y <- lapply(x, mean, na.rm=narm)))
stopifnot(identical(res13@outputs, "y"),
          identical(res13@inputs, c("x", "narm")),
          identical(res13@functions, c(lapply = NA, mean = NA)))

res13b = getInputs(quote(y <- lapply(FUN=mean, x, na.rm=narm)))
stopifnot(identical(res13b@outputs, "y"),
          identical(res13b@inputs, c("x", "narm")),
          identical(res13b@functions, c(lapply = NA, mean = NA)))

res14 = getInputs(quote(y <- apply(x,1, mean, na.rm=narm)))
stopifnot(identical(res14@outputs, "y"),
          identical(res14@inputs, c("x", "narm")),
          identical(res14@functions, c(apply = NA, mean = NA)))


res15 = getInputs(quote(y <- mapply(mean, x = stuff, y = things)))
stopifnot(identical(res15@outputs, "y"),
          identical(res15@inputs, c( "stuff", "things")),
          identical(res15@functions, c(mapply = NA, mean = NA)))



## do we catch updates correctly in all their various forms

res1 = getInputs(quote( x [ z > 0 ] <- 2 * y )) # outputs should be x and inputs x, z, y
stopifnot(identical(res1@updates, "x"),
          identical(res1@outputs, character()),
          identical(res1@inputs, c("x", "z", "y")))

res2 = getInputs(quote( foo(x) <- 1)) #updates and inputes are both "x"
stopifnot(identical(res2@inputs, "x"),
          identical(res2@outputs, character()),
          identical(res2@updates, "x"))

res3 = getInputs(quote( foo(x) <- a)) # updates is "x", inputs is x, a
stopifnot(identical(res3@inputs, c("x", "a")),
          identical(res3@updates, "x"),
          identical(res3@outputs, character()))



res4 = getInputs(quote( x$foo <- a)) 
stopifnot(identical(res4@inputs, c("x", "a")),
          identical(res4@updates, "x"),
          identical(res4@outputs, character()))

res5 = getInputs(quote( x[[foo]] <- a)) # outputs is "x", inputs is x, foo, a
stopifnot(identical(res5@inputs, c("x", "foo", "a")),
          identical(res5@outputs, character()),
          identical(res5@updates, "x"))

res6 = getInputs(quote( x[["foo"]] <- a)) # outputs is "x", inputs is x, a
stopifnot(identical(res6@inputs, c("x", "a")),
          identical(res6@strings, "foo"),
          identical(res6@updates, "x"),
          identical(res6@outputs, character()))

res8 = getInputs(quote(x[x>0] <- 5))
stopifnot(identical(res8@inputs, "x"),
          identical(res8@outputs, character()),
          identical(res8@updates, "x"))

res9 = getInputs(quote(x <- lapply(1:10, function(i) x[[10-i]])))
stopifnot(identical(res9@inputs, "x"),
          identical(res9@outputs, character()),
          identical(res9@updates, "x"))

## pipe handling and apply/map style function invocation play nicely
## together

res15 = getInputs(quote(1:10 %>% map_int(rnorm, sd = sample(1:10))))
stopifnot(identical(res15@inputs, character()))
stopifnot(identical(res15@functions, c("%>%" = NA, map_int = NA,
                                       rnorm = NA, sample = NA,
                                       ":" = NA)))
