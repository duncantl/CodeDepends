\name{runUpToSection}
\alias{runUpToSection}
\title{Evaluate the code blocks up to a particular section of a document}
\description{
 This function allows the caller to evaluate the code blocks within a
 document all the way up to a specified section of the document.
}
\usage{
runUpToSection(section, doc, all = TRUE, env = globalenv(),
                nestedEnvironments = FALSE, frags = readScript(doc),
                 verbose = FALSE)
}
\arguments{
  \item{section}{the index of the section, i.e. a number}
  \item{doc}{the name of the file containing the code}
  \item{all}{a logical value. It should be \code{TRUE} for now. }
  \item{env}{the environment in which the expressions will be evaluated}
  \item{nestedEnvironments}{a logical value controlling whether the
        each code block should be evaluated in its own environment which
	are created with the previous code block's environment as a
	parent environment.}
  \item{frags}{the code fragments read from the document or specified
      directly by the caller.}
  \item{verbose}{logical value indicating whether to display the code }
}
\value{
  A list containing the results of evaluating the different fragments.
  The list will have a length given by the section number.
}
\note{
  Currently, \code{all = FALSE} is not implemented.
}
\author{Duncan Temple Lang}
\seealso{
  \code{\link{sourceVariable}}
}
\examples{
 frags = parse(system.file("samples", "dual.R", package = "CodeDepends"))  
 runUpToSection(3, frags = frags, verbose = TRUE, all = TRUE)
}
\keyword{programming}
