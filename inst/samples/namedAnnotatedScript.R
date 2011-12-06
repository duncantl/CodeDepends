a = data({
n = 100
x = rnorm(n)
e = rnorm(n)
abc = 1  # irrelevant and should be discarded
})

b = data({
y = 3 + 10*x + e
})

sim = simulation({
a = 1:3
b = sum(a)
k = rpois(3, b)
})

graphics = plot(model({
fit = lm(y ~ x)
plot(fit)
}))

foo({
  # Here just to check the file dependencies
 source("foo.R")    
})

# bar(1:10)
