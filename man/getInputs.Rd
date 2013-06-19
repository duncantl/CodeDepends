\name{getInputs}
\alias{getInputs}
\alias{getInputs,ANY-method}
\alias{getInputs,Script-method}
\alias{getInputs,ScriptNode-method}
\alias{getInputs,ScriptNodeInfo-method}
\alias{getInputs,function-method}

\alias{scriptInfo}

\title{Get input and output variables and literals from R expressions}
\description{
  This function is used to analyze an R expression and
  identify the input and output variables in the expressions and
  related packages that are loaded and files that are referenced.

  This might be better called getCodeDepends. It is not to be confused
  with \code{getVariableInputs}.
}
\usage{
getInputs(e, collector = inputCollector(), basedir = ".", reset = FALSE, ...)
}
\arguments{
  \item{e}{the expression whose code we are to process}
  \item{collector}{an object which collects the different elements of
    interest in the code.}
  \item{basedir}{the directory for the code relative to which we can
    resolve file names.}
  \item{\dots}{additional parameters for methods}
  \item{reset}{a logical value that controls whether we call the
  collector's reset method before starting to process the expressions in
  the script.}
}

\value{
  A list with elements:
  \item{files}{the names of any strings used as arguments or literal
    values that correspond to file names.}
  \item{libraries}{the names of any libraries explicitly loaded within
    this code.}
  \item{inputs}{a character vector naming the variables that are used as
  inputs to the computations in this collection of expressions.}
  \item{outputs}{a character vector giving the names of the variables
    that are assigned values in this block of code, including
    assignments to elements of a variable, e.g. the variable x in the
    expression \code{x[[1]] <- 10}.}
  \item{functions}{a character vector naming the functions that are
    called within the code for this expression. This is not recursive,
    i.e. does not find the functions called by the function calls in
    this section.}  
}
%\references{}
\author{Duncan Temple Lang}

\seealso{
 \code{\link[base]{parse}}
}
\examples{
  frags = parse(system.file("samples", "dual.R", package = "CodeDepends"))  
  inputs = lapply(frags, getInputs)
  inputs
  sapply(inputs, slot, "outputs")

   # Specify the base directory in which to resolve the file names.
  getInputs(frags[[1]], basedir = system.file("samples", package = "CodeDepends"))


  f = system.file("samples", "namedAnnotatedScript.R", package = "CodeDepends")
  sc = readScript(f, "labeled")
  getInputs(sc)
  getInputs(sc[[2]])
}
\keyword{programming}
