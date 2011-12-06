{
  electionResults = read.table("myData")
}

{
  population = read.table("pop")
}

{
  census = read.table('ethnicity')
}


{
  f = foo()
  plot(electionResults, census, population)
}

{
  y = bar(f)
}

{
 B = plot(y)
}

{
 B = plot(y)
}

{
 z = plot(y, B)
}

{
 op1(z)
}

