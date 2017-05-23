\name{getVariableDepends}
\alias{getSectionDepends}
\alias{getVariableDepends}
\title{Determine dependencies for code blocks}
\description{
  These functions provide ways to determine which code blocks
  must be evaluated before others based on input and output variables.
  \code{getVariableDepends} is used to determine the code blocks
  that need to be run in order to define particlar variables.
  \code{getSectionDepends}
}
\usage{
getVariableDepends(vars, frags, info = lapply(frags, getInputs, ...),
checkLibraries = FALSE, asIndex = FALSE, functions = TRUE, ...)
getSectionDepends(sect, frags, info = lapply(frags, getInputs, ...), index =
FALSE, ...)
}
\arguments{
  \item{vars}{the names of the variables of interest}
  \item{frags}{the blocks or groups of expressions from the document}
  \item{info}{the information about the fragments that identify the
    inputs. This is typically computed as the default value for the
    parameter but can be provided explicitly when the caller has alreay
    computed this and passes it to different functions.}
  \item{index}{a logical value that controls whether we return the
    indices of the fragments of interest (\code{TRUE}) or return the
    fragments themselves (\code{FALSE})}
  \item{sect}{the index of the section/fragment to be analyzed}
  \item{checkLibraries}{a logical value}
  \item{asIndex}{a logical value that controls whether we return the
    expressions/code blocks or their indices.}
  \item{functions}{passed to \code{getVariables}. What kind of functions
    should be counted as variables (\code{TRUE} is local functions only,
    the default)}
  \item{...}{passed to \code{getInputs}. Ignored if \code{info} is
    explicitly specified.} 
}
\value{
  \code{getVariableDepends} returns a \code{\link{Script-class}} object
  consisting of the subset of code blocks relevant to the specified variables.
  
  If \code{asIndex} is \code{TRUE}, \code{getVariableDepends} returns the indices of the
  code blocks in the original script.
}
\author{Duncan Temple Lang}
\seealso{
  \code{\link{getPropagateChanges}}
  \code{\link{getExpressionThread}}   
}
\examples{
e = readScript(system.file("samples", "dual.R", package = "CodeDepends"))
getVariableDepends("fit", e, formulaInputs = TRUE)
getVariableDepends("fit", e, formulaInputs = TRUE, asIndex = TRUE)

getVariableDepends("y", e, asIndex = TRUE)
getVariableDepends("y", e)

}
\keyword{programming}
