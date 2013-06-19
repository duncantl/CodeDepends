x =  rnorm(100)
y = 3 + 7 * x + rnorm(length(x))

plot(x, y)
summary(x)
summary(y)

fit = lm(y ~ x)

d = data.frame(a = y, b = x)
fit1 = lm(a ~ b, d)
