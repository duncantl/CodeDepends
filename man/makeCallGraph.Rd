\name{makeCallGraph}
\alias{makeCallGraph}
\alias{makeCallGraph,list-method}
\alias{makeCallGraph,character-method}
\alias{makeCallGraph,function-method}
\title{Create a graph representing which functions call other functions}
\description{
  This function and its methods provide facilities for constructing
  a graph representing which functions call which other functions.
}
\usage{
makeCallGraph(obj, all = FALSE, ...)
}
\arguments{
  \item{obj}{The name of one or more packages as a string, optionally prefixed with
    \code{"package:"}. This can be a vector of package names.
    Currently the packages should already be on the search path.
    Other inputs may be supported in the future}
   \item{all}{a logical value that controls whether the graph includes
     all the functions called by any of the target functions. This will
     greatly expand the graph.}
  \item{\dots}{additional parameters for the methods}
}
\note{We may extend this to deal with global variables and methods}
\value{
 An object of class \code{\link[graph]{graphNEL-class}}
}
\author{Duncan Temple Lang}

\seealso{
  The \code{graph} and \code{Rgraphviz} packages.

  The \code{SVGAnnotation} package can be used to mae thee graphs interactive.
}
\examples{

  gg = makeCallGraph("package:CodeDepends")
  if(require(Rgraphviz)) {
     plot(gg, "twopi")

     ag = agopen(gg, layoutType = "circo", name = "bob")
     plot(ag)
  }

  if(require(Rgraphviz)) {
      # Bigger fonts.
    zz = layoutGraph(gg)
    graph.par(list(nodes = list(fontsize = 48)))
    renderGraph(zz)
  }

    # Two packages
    library(codetools)
    gg = makeCallGraph(c("package:CodeDepends", "package:codetools"))
}
\keyword{hplot}
\keyword{programming}
