
other = "LaTeX"
Targets = c("HTML", "FO", other)

create =
function(doc, target = Targets[1])
{
  makeDoc(doc, target)
}

create2 =
function(doc, target = foo(Targets))
{
  makeDoc(doc, target)
}
