\name{splitRedefinitions}
\alias{splitRedefinitions}
\title{Divide a script into separate lists of code based on redefinition
  of a variable}
\description{
  The purpose of this  function is to take a script consisting
  of individual calls or code blocks and to divide
  it into separate blocks in which a particular variable
  has only one definition.
  Within each block the variable is assigned a new value.
  
  At present, the code is quite simple and separates
  code blocks that merely alter an existing variable's
  characteristics, e.g. setting the names, an individual variable.
  Ideally we want to separate very different uses of
  a symbol/variable name which are unrelated. We will add more
  sophisticated code to (heuristically) detect such different uses,
  e.g. explicit assignments to a variable.

  Separating these code blocks can make it easier to treat the
  definitions separately and the different stages of the script.
}
\usage{
splitRedefinitions(var, info)
}
\arguments{
  \item{var}{the name of the variable whose redefinition will identify
  the different code blocks}
\item{info}{a list of \code{\link{ScriptNodeInfo-class}} objects
  identifying the input and output variables for each code block.}
}
\value{
  A list with as many elements as there are (re)definitions of the
  variable each being a list of code blocks.
}
\author{Duncan Temple Lang}
\seealso{
 \code{\link{readScript}}  
}
\examples{
 sc = readScript(system.file("samples", "redef.R", package =
  "CodeDepends"))
scinfo = getInputs(sc)
 groups = splitRedefinitions("x", scinfo)
}
\keyword{programming}
