x = rnorm(10)
total = 0
for(i in seq(2, length = length(x) - 1))
  if(x[i] > 2*x[i-1])
       total = total + 1
   
