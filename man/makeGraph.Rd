\name{makeVariableGraph}
\alias{makeVariableGraph}
\title{Create a graph describing the relationships between variables in
  a script}
\description{
  This creates a graph of nodes and edges describing the relationship
  of how some variables are used in defining others.
}
\usage{
makeVariableGraph(doc, frags = readScript(doc), info = getInputs(frags),
                   vars = getVariables(info, inputs = free), free = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{doc}{the name of the script file}
  \item{frags}{the code fragments from the script as a \code{Script} object.}
  \item{info}{the \code{ScriptInfo} list of \code{ScriptNodeInfo} objects describing each  node.}
  \item{vars}{a character vector giving the names of the variables in
    the scripts. By default, these are the variables defined in the script.}
  \item{free}{a logical value that is passed to \code{getInputs} and
    controls whether we include the free/global variables in the script.}
}
\details{
 Note that this collapses variables with the same name into a single
 node.  Therefore, if the code uses the same name for two
 unrelated variables, there may be some confusion.
}
\value{
 An object of class \code{graphNEL} from the \code{graph} package.
}
%\references{}
\author{Duncan Temple Lang}

\seealso{
  \code{\link{readScript}}
  \code{\link{getInputs}}    
  \code{\link{getVariables}}
  
  \code{graph}
  \code{Rgraphviz}
}
\examples{
\dontrun{
 u = url("http://www.omegahat.net/CodeDepends/formula.R")
 sc = readScript(u)
 close(u)
 g = makeVariableGraph(, sc)
}

 f = system.file("samples", "results-multi.R", package = "CodeDepends")
 sc = readScript(f)
 g = makeVariableGraph( info = getInputs(sc))
 if(require(Rgraphviz))
   plot(g)
}
\keyword{hplot}
\keyword{programming}
