\name{getFunctionDefs}
\alias{getFunctionDefs}
\title{Find function definitions in R code with static analysis}
\description{
  This function statically analyzes R code to find top-level function
  definitions. It follows \code{source} calls when the name of the file
  is a literal value in the call.
  It uses a path relative to the original R code source.
  It doesn't yet handle calls to \code{setwd}, or conditional sources
  or in a loop.
}
\usage{
getFunctionDefs(sc, envir = globalenv(), source = getSourceFunctions(sc, envir, byFile = FALSE))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sc}{R code as a language, \code{\linkS4class{Script}}, or \code{\linkS4class{ScriptInfo}} object.}
  \item{envir}{the environment in which to evaluate the function
    definition. This does not assign them to variables.}
  \item{source}{an additional collection of functions not
    found directly in \code{sc}.
    By default, these are found by following the \code{source}
    calls in \code{sc}. However, one can specify this
    manually if the script does not explicitly call
    \code{source} to define its functions as can be useful or essential.
  }
}
\value{
  a named list of functions. The names correspond
  to the variable names to which the functions would have been assigned.
}
\author{
Duncan Temple Lang
}
\examples{
f = system.file("samples", "varGraphEg2.R", package = "CodeDepends")
getFunctionDefs(f)

f = system.file("samples", "sourceEg.R", package = "CodeDepends")
getFunctionDefs(f)
}
% getFunctionDefs(f, byFile = TRUE)
\concept{static code analysis}

