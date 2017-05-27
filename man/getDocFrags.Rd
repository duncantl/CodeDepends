\name{readScript}
\alias{readScript}
\alias{readScript,character-method}
\alias{readScript,connection-method}
\alias{readScript,XMLInternalDocument-method}
\alias{readScript,missing-method}
\alias{readAnnotatedScript}
\title{Read the code blocks/chunks from a document}
\description{
  This is a general function that determines the type of the
  document and then extracts the code from it.

  This is an S4 generic  and so can be extended by other packages
  for document types that have a class, e.g. Word or OpenOffice
  documents.

  \code{readAnnotatedScript} is for reading scripts that use
  a vocabulary to label code blocks with high-level task identifiers
  to indicate what the code does in descriptive terms.
  
}
\usage{
readScript(doc, type = NA, txt = readLines(doc), ...)
readAnnotatedScript(doc, txt = readLines(doc)) 
}

\arguments{
  \item{doc}{the document, typically a string giving the file name. This
              can also be a connection, e.g. created via \code{\link[base]{url}}.}
  \item{type}{a string indicating the type of the document. If this is
    missing, the function calls \code{getDocType} to attempt to
  determine this based on the "common" types of documents.}
\item{txt}{the lines of text of the document.}
\item{...}{Passed to low-level input functions used by various methods.}
}
\value{
 A list of the R expressions that constitute the code blocks.
}

\author{Duncan Temple Lang }

\seealso{
\code{\link[base]{parse}}
}
\examples{
  e = readScript( system.file ("samples", "dual.R", package = "CodeDepends") )
  \dontrun{
  readScript(url("http://www.omegahat.net/CodeDepends/formula.R"))
}
}
\keyword{IO}
\keyword{programming}

