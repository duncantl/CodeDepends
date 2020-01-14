\name{getToplevelVariables}
\alias{getToplevelVariables}
\title{Get the Names of the Top-level Variables in R code}
\description{
  This statically analyzes R code and finds the names of the top-level
  assignments within it.
}
\usage{
getToplevelVariables(code, ...)
}
\arguments{
  \item{code}{the R code as a language, \code{\link{Script-class}} or \code{\link{ScriptInfo-class}}.}
  \item{\dots}{additional arguments, unused at present}
}
\value{
  a character vector.
}
\author{
Duncan Temple Lang
}
\note{
  It does not handle if(FALSE) statements, yet!
}
\seealso{
\code{\link{getPackages}}
}
\examples{
f = system.file("samples", "varGraphEg2.R", package = "CodeDepends")
getToplevelVariables(f)
}
\concept{static code analysis}
