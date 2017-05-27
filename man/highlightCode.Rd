\name{highlightCode}
\alias{highlightCode}
\title{Display R code with highlighting of variables, links to functions
and packages}
\description{
  This function leverages the \code{highlight} package to create
  an HTML display of R code.
  It connects all instances of a variable in the code  so that
  a viewer can move the mouse over a variable and see all uses of it
  in the code.

  The motivations for this is to help navigate a script and to allow us
  to connect the code to plots of, for example, the time-line or
  life-span of variables in a script.
}
\usage{

highlightCode(obj, out = NULL, addFunctionLinks = TRUE, checkURLs= TRUE,
              inline = TRUE, h = htmlRenderer(addFunctionLinks,
              checkURLs), css = system.file("CSS", "highlight.css",
              package = "CodeDepends"), jsCode =
              system.file("JavaScript", "highlightSymbols.js", package =
              "CodeDepends"))

}
\arguments{
  \item{obj}{the name of a file containing R code or an R expression or
    function.
    Currently, this needs to be a file.}
  \item{out}{the name of a file to which the HTML document is written,
  or \code{NULL} or \code{NA} to just return the in-memory document.}
  \item{addFunctionLinks}{how to generate the links for function
    calls. This can be \code{NULL} to have no links for function calls, or
    a logical value indicating whether to have links or not, or a
    function.
  If this is a function, it is called with a vector of  function names
  and should return a character vector with links for each of them.}
\item{checkURLs}{When sorting through possible link targets, should we
  check for existing local files OR URLs. Defaults to \code{TRUE}, if
  \code{FALSE} only locally existing files are checked for.}
  \item{inline}{a logical value indicating whether to put the CSS and
    JavaScript code directly into the HTML document or just refer to them.}
  \item{h}{the renderer to create the HTML. See \code{\link[highlight]{highlight}}}
  \item{css}{the URL or local file name for the CSS content}
  \item{jsCode}{the URL or local file name for the JavaScript code for
  the highlighting of the variables.}
}
\details{
  This uses the \code{\link[highlight]{highlight}} function
  to create the basic information for the code. We provide
  our own renderer to provide the links for function calls and
  packages and to specify markup for the symbols.
  Then we post-process the resulting HTML document to add our
  own CSS content and JavaScript code.
}
\value{
  An HTML document or the name of the file to which it was written if
  \code{out} is specified.
}
\author{
Duncan Temple Lang
}
\examples{
f = system.file("samples", "sitepairs.R", package = "CodeDepends")
## url checking takes a while, too long for CRAN example
highlightCode(f, "foo.html", checkURLs=FALSE)
}
\keyword{programming}

