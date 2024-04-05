\name{sourceVariable}
\alias{sourceVariable}
\title{Evaluate code in document in order to define the specified variables}
\description{
 This function allows the caller to evaluate the code within the
 document (or list of code chunks directly) in order to define
 one or more variables and then terminate. This is similar
 to \code{runUpToSection} but is oriented towards variables
 rather than particular code blocks.
}
\usage{
sourceVariable(vars, doc, frags = readScript(doc), eval = TRUE, env = globalenv(), 
                nestedEnvironments = FALSE, verbose = TRUE,
                 checkLibraries = eval, force = FALSE, first = FALSE,
                  info = lapply(frags, getInputs)) 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{vars}{the names of the variables which are of interest.
    This need not include intermediate variables, but instead is the
    vector of names of the variables that the caller wants defined ultimately.}
  \item{doc}{the document containing the code blocks}
  \item{frags}{the code fragments}
  \item{eval}{whether to evaluate the necessary code blocks or just
      return them.}
    \item{env}{the environment in which to evaluate the code blocks.}
    \item{nestedEnvironments}{a logical value indicating whether to
    evaluate each of the different code blocks within their own
    environment that is chained to the previous one.}
    \item{verbose}{a logical value indicating whether to print the
      expression being evaluated before it is actually evaluated.}
    \item{checkLibraries}{a logical value that controls whether we check
    for functions that are not currently available and if there are any
    whether we add calls to load libraries in \code{\link{getVariableDepends}}.}
    \item{force}{a logical value that controls whether we evaluate the
      expressions if they variables appear to exist.}
    \item{first}{a logical value.
    This is intended  to allow running up to the first instance of the variable, not all of them.}
    \item{info}{the information about each expression.  This is computed
    automatically, but the caller can specify it to avoid redundant computations.}
}
\value{
  If \code{eval} is \code{TRUE}, a list of the results
  of evaluating the code blocks. Alternatively,
  if \code{eval} is \code{FALSE}, this returns the expressions
  constituting the code blocks. In this case, the function is the same
  as \code{\link{getVariableDepends}}
}
%\references{}
\author{Duncan Temple Lang}
\note{
  We should add a \code{nestedEnvironments} parameter as in
  runUpToSection.  In fact, consolidate the code so it can be shared.
}
\seealso{
 \code{\link{getVariableDepends}}
}
\examples{
 f = system.file("samples", "dual.R", package = "CodeDepends")
 e = readScript(f)
 getVariableDepends("k", frags = e)
 sourceVariable("k", frags = e, verbose = TRUE)
}
\keyword{programming}
