\name{historyAsScript}
\alias{historyAsScript}
\title{Convert R interactive history to a Script object}
\description{
  This function is a means to capture the history
  of R commands  interactively entered at the prompt
  in this session (or saved across sessions)
  as a \code{\link{Script-class}} object.
  One can then analyze the expressions to find relationships
  between variables and commands,  which are irrelevant, ...
}
\usage{
historyAsScript()
}
\value{
 An object of class \code{\link{Script-class}}.
}
\author{
  Duncan Temple Lang
}
\seealso{
  \code{\link{readScript}}
  \code{\link[utils]{history}}
}
%\examples{}
\keyword{programming}
\concept{code analysis}

