\name{updatingScript}
\alias{updatingScript}
\alias{coerce,DynScript,Script-method}
\alias{getInputs,DynScript-method}

\title{Create a Script object that re-reads the original file as needed}
\description{
  This function reads the code in a particular document
  and creates a \code{\link{Script-class}} object
  to represent the code and allow us to do analysis
  on that code.
  Unlike \code{\link{readScript}}, this object
  continues to read any updates to the original code file
  when we use this \code{Script} object in computations.
  This allows us to modify the original source interactively
  and  concurrently with our R session and still have the
  script remain up-to-date  with that code.
}
\usage{
updatingScript(doc, ...)
}
\arguments{
  \item{doc}{the name/location of the document containing the R code}
  \item{\dots}{any additional arguments, passed to \code{\link{readScript}}.}
}
\details{
  This uses a reference class to update state across calls.
}
\value{
  an object of class \code{DynScript}
}
\author{
Duncan Temple Lang
}
\seealso{
 \code{\link{readScript}}
}
\examples{
cat("x = 1:10\ny = 3*x + 7 + rnorm(length(x))\n",  file = "foo.R")
sc = updatingScript("foo.R")

as(sc, "Script")

con = file("foo.R", "at")
cat("z = x + y", file = con)
close(con)

as(sc, "Script")
}
\keyword{programming}
\concept{code analysis}
