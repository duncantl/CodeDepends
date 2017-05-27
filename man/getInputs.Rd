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
getInputs (e, collector = inputCollector(), basedir = ".", reset =
  FALSE, formulaInputs = FALSE, ...)


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
  \item{formulaInputs}{ Logical indicating whether symbols appearing in
    formulas should be treated as inputs to the expression. Defaults to
    \code{FALSE}.}
  
  
}

\value{
  A ScriptInfo object containing information about the expression(s) in
e.

Things tracked include:
  \item{files}{the names of any strings used as arguments or literal
    values that correspond to file names.}
  \item{strings}{A vector of literal strings which appeared in \code{e}}
  \item{libraries}{the names of any libraries explicitly loaded within
    this code.}
  \item{inputs}{a character vector naming the variables that are used as
  inputs to the computations in this collection of expressions.}
  \item{outputs}{a character vector giving the names of the variables
    that are assigned values in this block of code, including
    assignments to elements of a variable, e.g. the variable x in the
    expression \code{x[[1]] <- 10}.}
  \item{updates}{ character vector of variables which receive new values
    when evaluating the expression, but must already exist. Note this
    does not currently catch some situations, so checking if any symbols
    appear in both inputs and outputs is still prudent.}
  \item{functions}{a named logical vector, where the names are the names
  of the functions called and the values indicate whether the function
  is local (TRUE), from a package (FALSE) or unknown (NA). Note that
  this is not recursive.}
  \item{removes}{a vector of variables which were removed (via the
    \code{rm} function) in \code{e}}
  \item{nsevalVars}{A vector of variables which appear in appear in
  \code{e}, but which are non-standarly evaluated and thus are not
  typical inputs. Note this classification is determined by the
  functionhandlers in use by \code{collector}.}
}
\note{Users should never call \code{getInputs.langauge} directly. It is
  listed here due to the vagaries of R CMD check documentation checks.}
%\references{}
\author{Duncan Temple Lang}

\seealso{
 \code{\link[base]{parse}}
}
\examples{
  frags = parse(system.file("samples", "dual.R", package = "CodeDepends"))  
  # formula involves non-df variables
  inputs = lapply(frags, getInputs, formulaInputs=TRUE)
  inputs
  sapply(inputs, slot, "outputs")

   # Specify the base directory in which to resolve the file names.
  getInputs(frags[[5]], basedir = system.file("samples", package = "CodeDepends"))


  f = system.file("samples", "namedAnnotatedScript.R", package = "CodeDepends")
  sc = readScript(f, "labeled")
  getInputs(sc)
  getInputs(sc[[2]])
}
\keyword{programming}
