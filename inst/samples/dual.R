{
n = 100
x = rnorm(n)
e = rnorm(n)
abc = 1  # irrelevant and should be discarded
}

{
y = 3 + 10*x + e
}

{
a = 1:3
b = sum(a)
k = rpois(3, b)
rm(a, b)
}

{
fit = lm(y ~ x)
plot(fit)
rm(y, x)
}

{
  # Here just to check the file dependencies
 source("foo.R")    
}

