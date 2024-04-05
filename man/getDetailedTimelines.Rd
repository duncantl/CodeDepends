\name{getDetailedTimelines}
\alias{getDetailedTimelines}
\alias{plot.DetailedVariableTimeline}
\alias{coerce,DetailedVariableTimeline,matrix-method}

\title{Compute and plot life cycle of variables in code}
\description{
  These functions allow one to get and visualize information about
  when variables are defined, redefined and used within
  and across blocks of code in a script or the body of a function.
}
\usage{
getDetailedTimelines(doc, info = getInputs(doc, ...), vars =
getVariables(info, functions = functions), functions=TRUE, ...)
\method{plot}{DetailedVariableTimeline}(x, var.srt = 0,
                                         var.mar = round(max(4,
.5*max(nchar(levels(x$var))))), var.cex = 1, main = attr(x, "scriptName"),...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{doc}{the name of a file or a connection which identifies the
    code to be analyzed}
  \item{info}{meta-information extracted from the code identifying the
    inputs and outputs. See \code{\link{getInputs}}.}
  \item{vars}{the variables of interest}
  \item{functions}{What type of functions should be included in the
    timeline: \code{NULL} for none, \code{TRUE} for locally defined
    only, \code{NA} for unknown provenance functions, or \code{FALSE}
    for non-locally-defined functions. Defaults to \code{TRUE}.}
  \item{x}{the \code{DetailedVariableTimeline} object being plotted}
  \item{var.srt}{rotation of the labels for the vertical axis listing
    the variables}
  \item{var.mar}{the number of lines to leave for the vertical axis.
   The labels for this are variable names so one often needs more space
   or to change the size of the labels.}
 \item{var.cex}{character expansion factor for the variable labels on
   
    the vertical axis.}
  \item{main}{the title of the plot}
  \item{\dots}{Passed to down to getInputs for the default \code{info}
  value in \code{getDetailedTimelines} and to underlying plotting
  functions for \code{plot.DetailedTimelines}.}
}
\value{
  \code{getDetailedTimelines} returns a data frame
  with four columns: \code{step}, \code{used}, \code{defined}, and
  \code{var}. Step represents steps within the timeline, with the same
  value indicating that the described event are occuring at the same
  time. \code{used} indicates whether \code{var} was used at that step,
  and \code{defined} indicates whether \code{var} was defined. Many rows
  will have \code{FALSE} for both as the variable is not used in that
  code block.
}
%\references{}
\author{Duncan Temple Lang}
\seealso{
  \code{\link{getInputs}}
}
\examples{
f = system.file("samples", "results-multi.R", package = "CodeDepends")
sc = readScript(f)
dtm = getDetailedTimelines(sc, getInputs(sc))
plot(dtm)
table(dtm$var)

 # A big/long function
info = getInputs(arima0)
dtm = getDetailedTimelines(info = info)
plot(dtm, var.cex = .7, mar = 4, srt = 30)
}
\keyword{programming}
\keyword{hplot}
