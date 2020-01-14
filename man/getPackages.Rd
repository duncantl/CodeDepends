\name{getPackages}
\alias{getPackages}
\alias{getPackages,Script-method}
\alias{getPackages,ScriptInfo-method}
\alias{getPackages,ScriptNodeInfo-method}
\alias{getPackages,character-method}
\alias{getPackages,expression-method}
\title{Get the packages used in R code}
\description{
  This functions collects the names of the
  R packages referenced in R code,
  via \code{library()}, \code{require},
  \code{::} or \code{:::}.
}
\usage{
getPackages(code, ...)
}
\arguments{
  \item{code}{R code as a language, \code{\linkS4class{Script}}, or \code{\linkS4class{ScriptInfo}} object.}
  \item{\dots}{additional arguments to be used in methods}
}
\value{
A character vector
}
\author{
Duncan Temple Lang
}
\seealso{
  \code{\link{getInputs}}
  \code{\link{readScript}}
}
\examples{
getPackages("library(foo); library(CodeDepends); require(utils)")
}
\concept{static code analysis}

