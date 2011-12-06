\name{guessTaskType}
\alias{guessTaskType}
\title{Guess the type of high-level task of a code block}
\description{
  This attempts to infer the type of the task being performed.
  There is a small set of known task types, listed in
  \code{system.file("Vocabulary", package = "CodeDepends")}.
  
  Currently this uses simple rules. In the future, we might
  use a classifier.
}
\usage{
guessTaskType(e, info = getInputs(e))
}
\arguments{
  \item{e}{the code block to be analyzed. This can be a call or an
    expression.
    Typically it is an element of a \link{Script-class}, i.e. a
   \code{ScriptNode-class} object}
  \item{info}{meta-information about the }
}
\value{
 A character vector giving the different task identifiers.
}
\author{Duncan Temple Lang}

\seealso{
 \code{\link{readScript}}  
}
\examples{
  guessTaskType(quote(plot(x, y)))

  e = expression({
          d = read.table("myData.txt")
          d$abc = d$a + log(d$b)
          d[ d$foo == 1, ] = sample(n)
      })
  guessTaskType(e)
}
\keyword{programming}

