\name{makeTaskGraph}
\alias{makeTaskGraph}
\title{Create a graph connecting the tasks within a script}
\description{
  This function create a graph connecting the high-level
  tasks within a script. The tasks are blocks of code that
  perform a step in the process.
  Each code block has input and output variables.
  These are used to define the associations between the tasks
  and which tasks are inputs to others and outputs that lead into
  others.
}
\usage{
makeTaskGraph(doc, frags = readScript(doc), info = as(frags, "ScriptInfo"))
}
\arguments{
  \item{doc}{the name of the script file}
  \item{frags}{the code blocks in the script}
  \item{info}{the meta-information detailing the inputs and outputs of
    the different code blocks/fragments}
}

\value{
 An object of class \code{\link[graph]{graphNEL-class}}.
}
%\references{}
\author{Duncan Temple Lang}

\seealso{
  \code{\link{readScript}}
  \code{\link{getInputs}}    
}
\examples{
\dontrun{
 f = system.file("samples", "dual.R", package = "CodeDepends")
 g = makeTaskGraph(f)

 if(require(Rgraphviz))
   plot(g)

 f = system.file("samples", "parallel.R", package = "CodeDepends")
 g = makeTaskGraph(f)

 if(require(Rgraphviz))
   plot(g)

 f = system.file("samples", "disjoint.R", package = "CodeDepends")
 g = makeTaskGraph(f)

 if(require(Rgraphviz))
   plot(g)
}
}
\keyword{programming}

