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
\alias{defhandler}
\alias{defaultFuncHandlers}
\title{Function handlers and handler factories}
\description{
  These functions act as handlers for individual call expressions, or in
  the case of factories, return functions which do.
}
\details{

  Function handlers should only be used to construct an input collector
  (i.e., as an argument to \code{inputCollector}). They should not ever
  be called directly by end users.

  When creating new function handlers, they should accept the arguments
  specified above (other than those to the factories).
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
  

