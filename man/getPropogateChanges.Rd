\name{getPropagateChanges}
\alias{getPropagateChanges}
\title{Determine which expressions to update when a variable changes}
\description{
  This function allows us to determine which subsequent expressions
  in the document need to be evaluated when a variable is assigned a new
  value. This is the "opposite" of  determining on which variables a
  given variable depends; this is for identifying which variables and
  expressions need to be updated when a variable changes.
  This is of use when propogating changes to dependent expressions.
}
\usage{

getPropagateChanges(var, expressions, info = lapply(expressions,
                    getInputs), recursive = FALSE, index = FALSE, envir
                    = globalenv(), eval = !missing(envir), verbose =
                    FALSE)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{var}{the name of the variable which has changed}
  \item{expressions}{the list of all expressions in the document}
  \item{info}{information extracted from the expressions about the
    inputs to each expressions. See \code{\link{getInputs}}.}
  \item{recursive}{a logical value that controls whether to work
    recursively on the expressions}
  \item{index}{a logical value which controls whether we return the
  indices of the expressions that would need to be evaluated based on
  the change to the variable \code{var}, or if \code{index} is
  \code{FALSE}, we return the expressions themselves.}
  \item{envir}{the environment in which to evaluate the expressions}
  \item{eval}{a logical value controlling whether we evaluate the
    expressions or just return them}
  \item{verbose}{a logical value that controls whether we output
    information about the expressions and their evaluation on the R console.}  
}
\value{
This returns
  either the expressions or the indices of the expressions
  that need to be re-evaluated due to a change in \code{var}.
}
\note{The returned expression do NOT include the expression which
  defines the variable \code{var}. Only expressions *after* that are
  included.
}
\author{Duncan Temple Lang}

\seealso{
  \code{\link{getExpressionThread}}
  \code{\link{getDependsThread}}
}
\examples{
sc = readScript(system.file("samples", "formula.R", package = "CodeDepends"))
info = getInputs(sc)
getPropagateChanges("x", sc, info = info)
getPropagateChanges("y", sc, info = info)
}
\keyword{programming}
