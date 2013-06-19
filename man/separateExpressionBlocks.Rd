\name{separateExpressionBlocks}
\alias{separateExpressionBlocks}
\title{Convert a script into individual top-level calls}
\description{
  This function converts a script of code blocks (e.g. from Sweave, XML,
  or an annotated script) with grouped expressions into
  individual top-level calls.
  The intent of this is to allow us to deal with the calls at a
  higher-level
  of granularity than code blocks.
  In other words, we can easily compute the depenendcies
  on the individual calls rather than on collections
  of calls. This allows us to re-evaluate individual expressions rather
  than entire code blocks when we have to update variables due to
  changes in "earlier" variables, i.e. those defined earlier in the
  script   and recomputed for various reasons.
}
\usage{
separateExpressionBlocks(blocks)
}
\arguments{
  \item{blocks}{a list of the expressions or calls, i.e. the code
    blocks, in the script.}
}
\value{
 A list of call or assignment expressions.
}
%\references{}
\author{Duncan Temple Lang}

\seealso{
 \code{\link{readScript}}  
}
\examples{
 f = system.file("samples", "dual.R", package = "CodeDepends")
 sc = readScript(f)
 separateExpressionBlocks(sc)
}
\keyword{programming}
\keyword{IO}

