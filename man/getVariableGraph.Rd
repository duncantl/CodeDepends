\name{getVariableGraph}
\alias{getVariableGraph}
\alias{getVarDependsGraph}

\title{Create dependency graph for a variable}
\description{
  This analyzes the R code and finds the
  definition of the specified variable
  and then determines which other variables
  are required to compute this definition.
  In this way, we have the variables which are directly and
  indirectly used to create \code{var}.
}
\usage{
getVariableGraph(var, sc, asGraph = TRUE, globals = TRUE, idx = getVariableDepends(var, sc, asIndex = TRUE, functionGlobals = functionGlobals), functionGlobals = getFunctionGlobals(sc))
}
\arguments{
  \item{var}{a character vector of variable names created in the R code}
  \item{sc}{R code as a language, \code{\linkS4class{Script}}, or \code{\linkS4class{ScriptInfo}} object.}
  \item{asGraph}{a logical controlling whether to return an
    \code{igraph} graph, or the data frame describing the edges.}
  \item{globals}{ignored. Will be removed}
  \item{idx}{the indices of the top-level expressions/elements in
    \code{sc} corresponding to the expressions on which \code{var}
    depends directly or indirectly. See \code{\link{getVariableDepends}}}
  \item{functionGlobals}{\code{NULL} or an empty list, or a named list of character vectors identifying
    the functions that use global variables and the names of those
    global variables. See \code{\link{getFunctionGlobals}}.}
}
\value{
  either an \code{igraph} graph object
  or a two column data frame providing the
  edges for the graph. %XXX
}
\author{
  Duncan Temple Lang
}

\seealso{
  \code{\link{getVariableDepends}}
  \code{\link{getFunctionGlobals}}
}
\examples{

f = system.file("samples", "varGraphEg2.R", package = "CodeDepends")
sc = readScript(f)
g1 = getVariableGraph("r", sc, functionGlobals = NULL)

globals = getFunctionGlobals(sc)
g2 = getVariableGraph("r", sc, functionGlobals = globals)
}
\concept{static code analysis}

