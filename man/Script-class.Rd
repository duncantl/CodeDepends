\name{Script-class}
\docType{class}
\alias{Script-class}
\alias{AnnotatedScript-class}
\alias{ScriptNode-class}
\alias{ScriptInfo-class}
\alias{ScriptNodeInfo-class}
\alias{coerce,Script,ScriptInfo-method}
\alias{coerce,ScriptNode,ScriptNodeInfo-method}
\alias{coerce,expression,ScriptNodeInfo-method}
\alias{coerce,language,ScriptNodeInfo-method}
\alias{[,Script,vector,missing-method}
\alias{[,Script,character,missing-method}
\alias{$,Script-method}

\title{The Script class and elements}
\description{
  This package works with collections of expressions
  or code blocks and such a sequence can be thought of as a script.
  The \code{Script} class is a list of code elements.
  Such objects are typically created via a call to
  \code{\link{readScript}}.
  They can be read from XML files, tangled Sweave output,
  regular R source files and R source files that are annotated
  to identify the general task of each code block.
  This last type of script has its own class named
  \code{AnnotatedScript} and the code elements are annotated with labels
  such as dataInput, simulate, plot, model, eda and so on.

  Each element of a \code{Script} list represents code.
  These are stored as objects of class \code{ScriptNode}.
  A \code{ScriptNode} class has slots for the
  \code{code}, the \code{taskType} indicating the high-level nature of
  the code, and an \code{id} so we can easily refer to it.

  While our focus is on the code elements in a \code{Script}, we work
  with meta-data about the code elements. We identify information such
  as the input variables required by a code element, the variables it
  assigns (the outputs) and so on.  This information is stored
  in a \code{ScriptNodeInfo} object.
  And a collection of such objects that parallels a script
  is a \code{ScriptInfo} object.

  We can easily map a \code{Script} or a \code{ScriptNode}
  to the corresponding meta-information via the coercion methods
  \code{as(script, "ScriptInfo")} and  \code{as(node, "ScriptNodeInfo")}.
}
\section{Objects from the Class}{
  Objects of class \code{Script} are created with
  \code{\link{readScript}}.

  Objects of class \code{ScriptInfo} are created with
  \code{\link{getInputs}} or \code{as(, "ScriptInfo")}.

}
\section{Slots}{
	 \describe{
    \item{\code{.Data}:}{the elements of the list.}
    \item{\code{location}:}{a character string that gives the file name
      or URL of the code for this script.}
  }
}
\section{Extends}{
Class \code{"\linkS4class{list}"}, from data part.
Class \code{"\linkS4class{vector}"}, by class "list", distance 2.
}
\section{Methods}{
  \describe{
    \item{coerce}{\code{signature(from = "Script", to = "ScriptInfo")}:
      convert a \code{Script} to a \code{ScriptInfo} to access the meta-information}
    \item{coerce}{\code{signature(from = "ScriptNode", to =
	"ScriptNodeInfo")}: 
      compute the meta-information from an individual code element.}
	 }
}
\author{ Duncan Temple Lang}
\seealso{
 \code{\link{readScript}}
}
\examples{
 f = system.file("samples", "results-multi.R", package = "CodeDepends")
 sc = readScript(f)
 info = as(sc, "ScriptInfo")
 info = getInputs(sc, basedir = dirname(f))

   # Providing our own handler for calls to source()
 sourceHandler = function(e, collector = NULL, basedir = ".", ...) {
     collector$string(e[[2]], , TRUE)
     collector$calls(as.character(e[[1]]))
 }
 h = CodeDepends:::inputCollector(source = sourceHandler)
 info = getInputs(sc, h, basedir = dirname(f))

\dontrun{
 u = url("http://www.omegahat.net/CodeDepends/formula.R")
 sc = readScript(u)
 as(sc, "ScriptInfo")
}
}
\keyword{classes}
\keyword{programming}

