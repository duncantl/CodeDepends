\name{getVariables}
\alias{getVariables}
\alias{getVariables,Script-method}
\alias{getVariables,ScriptInfo-method}
\alias{getVariables,ScriptNode-method}
\alias{getVariables,ScriptNodeInfo-method}
\alias{getVariables,expression-method}
\title{Get the names of the variables used in code}
\description{
  These functions and methods allow one to get the
  names of the variables used within a script or block
  of code and from various derived types.
}
\usage{
getVariables(x, inputs = FALSE, ...)
}
\arguments{
  \item{x}{the object with information about the variables}
  \item{inputs}{a logical indicating wether to include the input
    variables or just return the output variables, i.e. those on the
    left hand side of of an assignment.}
  \item{\dots}{any additional parameters for methods}
}
\value{
  A character vector, with possibly repeated values,
  giving the names of the variables.
}
\author{Duncan Temple Lang}
\seealso{
  \code{\link{readScript}}
  \code{\link{getInputs}}    
}
\examples{
 f = system.file("samples", "namedAnnotatedScript.R", package = "CodeDepends")
 sc = readScript(f, "labeled")
 getVariables(sc)

 getVariables(sc[[3]])
}
\keyword{programming}
