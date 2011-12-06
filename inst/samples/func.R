{
 x = 1:10
 y = x + 3 + rnorm(length(x))
}

{
 fun = function(x, y) {
    glm( y ~ x)
 }
}

{
  sapply(1:10, function(x) glm(x + i))
}
