library(CodeDepends)


res7 = getInputs(quote(x <- names(y[a:5])[w]))
stopifnot(identical(res7@inputs, c("y", "a", "w")),
          identical(res7@outputs, "x"),
          identical(res7@code, quote(x <- names(y[a:5])[w])))


## do we catch updates correctly in all it's various forms
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
