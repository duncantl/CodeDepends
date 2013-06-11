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
getDetailedTimelines(doc, info = getInputs(doc), vars = getVariables(info))
\method{plot}{DetailedVariableTimeline}(x, var.srt = 0,
                                         var.mar = round(max(4, .5*max(nchar(levels(x$var))))), var.cex = 1, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{doc}{the name of a file or a connection which identifies the
    code to be analyzed}
  \item{info}{meta-information extracted from the code identifying the
    inputs and outputs. See \code{\link{getInputs}}.}
  \item{vars}{the variables of interest}
  \item{x}{the \code{DetailedVariableTimeline} object being plotted}
  \item{var.srt}{rotation of the labels for the vertical axis listing
    the variables}
  \item{var.mar}{the number of lines to leave for the vertical axis.
   The labels for this are variable names so one often needs more space
   or to change the size of the labels.}
  \item{var.cex}{character expansion factor for the variable labels on
    the vertical axis.}
  \item{\dots}{additional arguments to the \code{plot} command. These
    might include, for example, main to put a title on the plot.}
}
\value{
  \code{getDetailedTimelines} returns a data frame
  with three variables: \code{var}, \code{used} and \code{defined}.
  For each variable, there are as many rows as there are code blocks in
  the document (and elements in \code{info}). (Variables that are
  redefined will have more rows, but these are essentially different
  variables.)
  These rows correspond to the different code blocks or "time steps".
  \code{used} and \code{defined} indicate whether the variable
  acted as an input or was defined within this code block.
  Many will have \code{FALSE} for both as the variable is not used in
  that code block.
  \code{var} is used merely to identify the variable.
}
%\references{}
\author{Duncan Temple Lang}
\seealso{
  \code{\link{getInputs}}
}
\examples{
f = system.file("samples", "results-multi.R", package = "CodeDepends")
sc = readScript(f)
dtm = getDetailedTimelines(, getInputs(sc))
plot(dtm)
table(dtm$var)

 # A big/long function
info = getInputs(arima0)
dtm = getDetailedTimelines(, info)
plot(dtm, var.cex = .7, mar = 4, srt = 30)
}
\keyword{programming}
\keyword{hplot}
