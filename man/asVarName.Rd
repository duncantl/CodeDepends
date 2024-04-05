\name{asVarName}
\alias{asVarName}
\title{
asVarName
}
\description{
This function grabs a symbol out of an expression and returns it as a
character (see details for which symbol will be used).

This is a convenience function for use when constructing custom function
handlers, it's unlikely to have much utility outside of that context.
}
\usage{
asVarName(x)
}
\arguments{
  \item{x}{The (sub)expression to extract a symbol from
}
}
\details{
This function always returns a character vector representing a single
symbol from \code{x}, but which code varies depending on the exact form
of \code{x}. When
\describe{
\item{\code{x} is a single symbol}{the character representation of the symbol is returned}
\item{\code{x} is a function call}{asVarName is recursively called on
  the sub-expression for the first argument}
\item{\code{x} is an assignment}{asVarName is called recursively on the
  right-hand side (after \code{->} expressions are transformed to
  \code{<-} ones). This is a special case of the rule above.}
}
}
\value{
A character vector of length one representing the symbol (or literal) as
described in the Details section.
}
\author{
Duncan Temple Lang
}
\examples{
asVarName(quote(rnorm(x, y, z))) # "x"
asVarName(quote(rnorm(x, y, z))[[1]] ) # "rnorm" b/c [[1]] is called fun
asVarName(quote(rownames(a) <- b )) # "a"
asVarName(quote(rnorm(10, y, z))) # "10"
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ utitlites }% use one of  RShowDoc("KEYWORDS")
\keyword{ programming }% __ONLY ONE__ keyword per line
\keyword{ misc }
