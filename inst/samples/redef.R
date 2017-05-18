x = 1:10
x = c(1, x, 10)
y = sample(x, 12)

cor(x, y)

x = rnorm(12)

cor(x, y)
