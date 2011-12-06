x = 1:10
y = 10 + 3*x + rnorm(length(x))
z = 3

hist(x)

fit = lm(y ~ x)
plot(fit)
summary(fit)
coef(fit)




