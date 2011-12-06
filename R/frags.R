#
# This file contains functions to get the fragments/blocks/expressions
# from different types of documents.
# These could quite happily live in other packages, but are put here for
# now just for a start.
# There is code in other packages (XDynDocs) for, e.g., reading code chunks from Word documents.
#

readRExpressions =
function(doc, txt = readLines(doc))
{
  old = options()
  on.exit(options(old))
  options(keep.source = FALSE)  

  parse(text = txt)
}

setGeneric("readScript",
            function(doc, type = NA, txt = readLines(doc))
             standardGeneric("readScript"))

tmp =
function(doc, type = NA, txt = readLines(doc))
{
  if(is.na(type) ) 
    type = getDocType(doc, txt)

  ans = frag.readers[[type]](txt = txt)
  if(!is(ans, "Script"))
    ans = new("Script", ans, location = if(missing(doc))
                                           as.character(NA)
                                        else if(inherits(doc, "connection"))
                                           summary(doc)$description
                                        else doc)

  ans
}

setMethod("readScript", "character", tmp)
setOldClass("connection")
setOldClass(c("url", "connection"))
setOldClass(c("file", "connection"))
setMethod("readScript", "connection", tmp)


getDocType =
function(doc, txt = readLines(doc))
{

  if(length(grep("<([[:alpha:]]*:code|code)", txt)))
      "xml"
  else if(length(grep("^### chunk number", txt)))
      "Stangled"
  else
      "R"
}

readAnnotatedScript =
  #
  # Read a script that consists of labeled blocks of code
  # where the labels identify the nature of the task for a code block.
  #
function(doc, txt = readLines(doc))
{
  e = parse(text = txt)
  nodes = lapply(e, ScriptNode)
  ans = new("AnnotatedScript", nodes, location = if(missing(doc))
                                           as.character(NA)
                                        else if(inherits(doc, "connection"))
                                           summary(doc)$description
                                        else doc)
  ids = sapply(ans, slot, "id")
  names(ans)[!is.na(ids)] = ids[!is.na(ids)]
  ans
}

ScriptNode =
  #
  # Create a script node which should be  of the form
  #  label(label(label( {    })
  # or
  #  id = label(label(label({ ... })))
  # The labels are vocabulary to identify the nature of the task of the block.
  # id is a name for the block.
  #
function(expr)
{
  task = character()
  id = NA
  e = expr
  if(is.call(e) && as.character(e[[1]]) %in% c("<-", "=", "<<-")) {
    id = as.character(e[[2]])
    expr = expr[[3]]
  }
  
  while(class(expr) != "{") {
    task = c(task, as.character(expr[[1]]))
    expr = expr[[2]]
  }
  new("ScriptNode", code = expr, taskType = task, id = as.character(id))
}


frag.readers =
  # list of functions indexed by a document type string
  # so that we can determine the type of the document
  # and then figure out how to get the fragments.
  list( xml = function(doc, txt = readLines(doc)) xmlSource(txt, asText = TRUE, eval = FALSE),
        Stangled = getTangledFrags,
        R = readRExpressions,
        labeled = readAnnotatedScript)


