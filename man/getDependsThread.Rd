\name{getDependsThread}
\alias{getDependsThread}
\alias{getDependsThread,character-method}
\alias{getDependsThread,name-method}
\alias{getDependsThread,numeric-method}
\title{Compute which code blocks in a script are inputs to define a variable}
\description{
  This function is used to determine which code blocks in an R "script"
  that are needed to define a particular variable.
  This finds the smallest complete set of expressions or code blocks
  that must be evaluated in order to define the specified
  variable(s).  It omits expressions that do not provide
  outputs that are not used as inputs  to (indirectly) define
  the speciied variable.
}
\usage{
getDependsThread(var, info, reverse = TRUE)
}
\arguments{
  \item{var}{the name of a variable in the script}
  \item{info}{a list of the meta-information for each of the
    code elements in the script.
  }
  \item{reverse}{a logical value that determines whether
    we reverse the indices of the expressions
    or leave them as end-to-first.
  }
}
\value{
  An integer vector giving the indices of the
  script code blocks which are required to define
  \code{var}.
}
\author{Duncan Temple Lang}
\seealso{
  \code{\link{getExpressionThread}}
  \code{\link{readScript}}
  \code{\link{getVariables}}
}
\examples{
 sc = readScript(system.file("samples", "dual.R", package =
"CodeDepends"))
sci = getInputs(sc, formulaInputs = TRUE) ## script has formula with no data.frame
 idx = getDependsThread("fit", sci)
}
\keyword{programming}
\concept{meta-programming}
