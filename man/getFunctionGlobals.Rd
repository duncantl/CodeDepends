\name{getFunctionGlobals}
\alias{getFunctionGlobals}
\alias{getGlobalInputs}
\title{Get the names of global variables used in R functions in an R script}
\description{
  This statically analyzes the code in an R script and finds the
  function definitions in it, following \code{source} calls (with
  explicit/literal file names) to find additional code.
  It then determines the global variables used in these functions.
  It does not assign the functions to variable names, but merely
  instantiates them as functions for analysis.

  \code{getGlobalInputs} optionally uses the global variables
  from \code{getFunctionGlobals}
  to add the global variables from a function to the dependencies
  in a call to that function.
  This is important in \code{\link{getVariableGraph}} to show
  the implicit dependencies on global variables.
}
\usage{
getFunctionGlobals(sc, funs = getFunctionDefs(sc, envir), envir = globalenv(), getGlobals = function(x) codetools::findGlobals(x, FALSE))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sc}{R code as a language, \code{\link{Script-class}}, or \code{\link{ScriptInfo-class}} object.}
\item{funs}{the list of all the functions in the R code.
 These are found via static analysis to avoid having to
 \code{\link[base]{source}} code that may perform computations
 other than defining functions.
}
  \item{envir}{the environment in which to evaluate the function definition.}
\item{getGlobals}{
  a function to compute the global variables in a function.
  By default, we use \code{\link[codetools]{findGlobals}}.
  However, the function \code{getGlobals}
  in the \code{CodeAnalysis} package is more general and covers
  more cases in which \code{\link[codetools]{findGlobals}} gives
  incorrect results.}
}

\value{
  a named list where the names identify the functions
  and each element is a character vector identifying
  the names of the variables in that function that
  are global/non-local variables.
}
\author{
Duncan Temple Lang
}

\seealso{
  \code{\link{getFunctionDefs}}
}
\examples{
f = system.file("samples", "varGraphEg2.R", package = "CodeDepends")
getFunctionGlobals(f)
}
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")

