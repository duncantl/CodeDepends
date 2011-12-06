{
 
n = 20
shape1 = .1
shape2 = .5
F = function(n) rbeta(n, shape1, shape2)
 
}

{
 
R = 100
samples = replicate(R, F(n), simplify = FALSE)
T = sapply(samples, max)
 
}

{
 
hist(T, xlim = c(0, 1), prob = TRUE, main = paste("n =", n, ",a =", shape1, ", b =", shape2))
curve(dbeta(x, shape1, shape2), 0, 1, add = TRUE, col = "red")
 
}

{
 
boxplot(samples, ylim = c(0, 1))
 
}
