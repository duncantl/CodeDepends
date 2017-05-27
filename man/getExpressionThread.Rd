\name{getExpressionThread}
\alias{getExpressionThread}
\title{Find the sequence of expressions needed to get to a certain point in the code}
\description{

 What's the difference between this and getVariableInputs,
 getVariableDepends, getSectionDepends?
  
  This does not currently attempt to get the minimal subset of expressions
  within the code block. In other words, if there are extraneous
  expressions
  within these blocks that are not actually necessary, these are
  evaluated. This is important for expressions with side effects, e.g.
  writing files or generating plots.
  
}
\usage{
getExpressionThread(target, expressions, info = lapply(expressions,
getInputs, ...), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{target}{either the index of the
    expression of interest in \code{expressions} or the names of the variables.}
  \item{expressions}{the list of expressions}
  \item{info}{a list of objects giving information about the inputs
    to each top-level expression in expressions.}
  \item{...}{Passed to \code{getInputs} if \code{info} is not explicitly
    specified.}
  
}
\value{
  A \code{\link{Script-class}} object containing the subset of the
  code chunks pertinent to the target variable(s).
}
%\references{}
\author{Duncan Temple Lang}
\seealso{
  \code{\link{getDependsThread}}
}
\examples{
 e = readScript(system.file("samples", "dual.R", package = "CodeDepends"))
 getExpressionThread("fit", e)

 getExpressionThread("y", e)
 getExpressionThread("x", e)

 getExpressionThread("k", e)

  # With several
 s = readScript(system.file("samples", "sitepairs.R", package = "CodeDepends"))
 o = getExpressionThread("covs", s)
}
\keyword{programming}
