\name{funchandlers}
\alias{libreqhandler}
\alias{rmhandler}
\alias{dollarhandler}
\alias{assignhandler}
\alias{assignfunhandler}
\alias{funchandler}
\alias{formulahandler}
\alias{fullnsehandler}
\alias{nseafterfirst}
\alias{nsehandlerfactory}
\alias{groupbyhandler}
\alias{counthandler}
\alias{spreadhandler}
\alias{colonshandler}
\alias{pipehandler}
\alias{forhandler}
\alias{datahandler}
\alias{filterhandler}
\alias{funshandler}
\alias{applyhandlerfactory}
\alias{summarize_handlerfactory}
\alias{noophandler}
\alias{defhandler}
\alias{defaultFuncHandlers}
\alias{functionhandlers}
\title{Specifying custom processing behavior, Function handlers and handler factories}
\description{
  Custom behavior when processing calls to certain functions is
  implemented by specifying \emph{function handlers} for those
  functions. This can be used to alter CodeDepends' behavior when it
  sees these functions, or if desired, to ignore them entirely when
  processing the parent expression.

  Function handlers should never be called directly by end users.
  
  CodeDepends attempts to provide reasonable defaults, in the form of
  the \code{defaultFuncHandlers} list, which should be suitable for most
  users.

}
\details{

  Custom handling of functions and, rarely, some types of non functions
  (currently only inlined \code{NativeSymbol} objects) by the
  \code{getInputs} function is specified via function handlers, which
  are passed in a named list to \code{inputCollector} when creating a
  collector for use by \code{getInputs}.
  
  Function handlers should only be used to construct an input collector
  (i.e., as an argument to \code{inputCollector}). They should not ever
  be called directly by end users.

  When creating new function handlers, they should accept the arguments
  specified above (other than those to the factories). The first
  argument, e, will be an expression representing a call to the function
  the handler is specified for, and second \code{collector} will be the
  collector object. Handlers are expected to recursively process all
  aspects of the call expression to the extent desired. This will often
  be done by calling getInputs again on, e.g., some or all arguments
  passed into the function call.

  Function handlers are also expected to respect the \code{pipe} and
  \code{nseval} arguments they receive. 
}
\arguments{
  \item{e}{The (sub)expression being processed. This will be a call to
    the function your handler is assigned to work on.}
  \item{collector}{The input collector in use. Represents state as the
  expression tree is walked.}
  \item{basedir}{The base directory when checking if a string literal is
    a file path}
  \item{input}{Are we in a part of the whole expression that specifies
    inputs}
  \item{formulaInputs}{Are symbols within formulas to be counted as
    inputs (\code{TRUE}) or non-standardly evaluated variables
    (\code{FALSE})}
  \item{update}{Are we in a part of the expression that indicates a
    variable's value is being updated (i.e., complex right hand side)}
  \item{pipe}{Are we in a direct pipe call}
  \item{nseval}{Should any symbols that appear to be inputs be treated
    as nonstandardly-evaluated instead}
  \item{...}{unused}
}
  

