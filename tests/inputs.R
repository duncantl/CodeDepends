library(CodeDepends)
getInputs(quote( x [ z > 0 ] <- 2 * y )) # outputs should be x and inputs x, z, y

getInputs(quote( foo(x) <- 1)) # outputs is "x"

getInputs(quote( foo(x) <- a)) # outputs is "x", inputs is a


getInputs(quote( x$foo <- a)) # outputs is "x", inputs is x, a

getInputs(quote( x[[foo]] <- a)) # outputs is "x", inputs is x, foo, a

getInputs(quote( x[["foo"]] <- a)) # outputs is "x", inputs is x, a
