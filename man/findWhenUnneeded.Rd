\name{findWhenUnneeded}
\alias{findWhenUnneeded}
\alias{addRemoveIntermediates}
\title{Determine the code block after which a variable can be explicitly
removed}
\description{
  These functions analyze the meta-information about code blocks
  and determine when a variable is no longer needed and
  can add code to the relevant code block to remove the variable. 
}
\usage{
findWhenUnneeded(var, frags, info = lapply(frags, getInputs), simplify,
                 index = TRUE, end = NA, redefined = FALSE)
addRemoveIntermediates(doc, frags = readScript(doc),
                       info = getInputs(frags),
                       vars = getVariables(info))
}
\arguments{
\item{var}{the name of the variable(s) whose final } \item{doc}{the
location of the script, given as a file name or a connection}
\item{frags}{an object of class \code{Script} which is a list containing
the code blocks in the script. This is typically obtained via a call to
\code{\link{readScript}}.}  \item{info}{an object of class
\code{ScriptInfo} which is a list of \code{ScriptNodeInfo} objects.}
\item{simplify}{ignored} \item{index}{a logical value indicating whether
\code{findWhenUnneeded} should return the indices of the code
blocks/fragments or the code fragments themselves.}  \item{vars}{the
names of all the variables of interest} \item{end}{the value to use if
the variable is used in the last code block, i.e. the end of the
script.}  \item{redefined}{a logical value which controls whether we
return the earliest code block in which the variable is redefined rather
than when the variable is no longer used.  Redefinition is a kind of
"no longer being used" but for the value, not the variable.  } }
%\details{}
\value{
  A vector of indices indicating the last expression in which each of
  the specified  variables is an input.
}
%\references{}
\author{Duncan Temple Lang}

\seealso{
  \code{\link{readScript}}
  \code{\link{addRemoveIntermediates}}
}
\examples{
f = system.file("samples", "cleanVars.R", package = "CodeDepends")
sc = readScript(f)
findWhenUnneeded("x", sc)
findWhenUnneeded(c("x", "y"), sc)  

# z is never used
findWhenUnneeded("z", sc)
findWhenUnneeded("z", sc, end = 1L)  

code = addRemoveIntermediates(f)
 # Note that rm(x), rm(y) and rm(d) are added.
code[c(4, 5, 6)]
}
\keyword{programming}
