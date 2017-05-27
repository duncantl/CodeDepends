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

setGeneric("readScript", signature = c("doc", "txt"), 
            function(doc, type = NA, txt = readLines(doc), ...)
             standardGeneric("readScript"))

tmp =
function(doc, type = NA, txt = readLines(doc), ...)
{
  if(is.na(type) ) 
    type = getDocType(doc, txt)

  ans = frag.readers[[type]](doc, txt = txt, ...)
  if(!is(ans, "Script"))
    ans = new("Script", ans, location = if(missing(doc))
                                           as.character(NA)
                                        else if(inherits(doc, "connection"))
                                           summary(doc)$description
                                        else doc)

  if(any(duplicated(names(ans)))) 
    names(ans) = makeDuplicatedNames(names(ans))


  ans
}

##setMethod("readScript", c("missing", txt = "character"),
setMethod("readScript", c("missing", txt = "ANY"),
          function(doc, type = NA, txt = readLines(doc), ...)
            tmp(type = type, txt = txt, ...)
         )

setMethod("readScript", "character", tmp)

setMethod("readScript", "XMLInternalDocument",
           function(doc, type = NA, txt = readLines(doc), ...) {
             readXMLScript(doc, ...)
           })

setOldClass("connection")
setOldClass(c("url", "connection"))
setOldClass(c("file", "connection"))
setMethod("readScript", "connection", tmp)



getDocType =
function(doc, txt = readLines(doc))
{

    if(is(txt, "list") && is.language(txt[[1]]))
        "elist"
    else if(is.language(txt))
        "language"
    else if(length(grep("<([[:alpha:]]*:code|code)", txt)))
        "xml"
    else if(any(grepl("^(### chunk number|<<[^>]*>>=|```\\{r.*\\})", txt)))
        "Stangled"
    else if (any(grepl("^#\\+BEGIN_SRC R", txt, ignore.case=TRUE)))
        "org"
    else if(is(txt,"character")) ## this is txt, so can't be connection.
        "R"
    else
        stop("Unable to determine document type.")
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
  nms = rep("", times = length(ans))
  nms[!is.na(ids)] = ids[!is.na(ids)]
  ## we need suppress warnings here because there is a (seemingly incorrect)
  ## warning saying that assigning names here will create an invalid object.
  if(any(nzchar(nms)))
      suppressWarnings({names(ans) = nms})
  

  #  names(ans)[!is.na(ids)] = ids[!is.na(ids)]
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


readXMLScript =
function(doc, txt = readLines(doc), ...)
{
  if(is.character(doc))
    doc = xmlParse(doc)
  
#  val = xmlSource(txt, asText = TRUE, eval = FALSE, setNodeNames = TRUE)
  val = xmlSource(doc, eval = FALSE, setNodeNames = TRUE, ...)  
  i = grep("^r:(code|plot|function|init)", names(val))
  if(length(i)) {
    names(val)[i] = sprintf("step %d", i)
  }
  val  
}


readOrgmode =
    function(doc, txt = readLines(doc), ...)
{
    
    stop("notimplemented")

}
frag.readers =
  # list of functions indexed by a document type string
  # so that we can determine the type of the document
  # and then figure out how to get the fragments.
  list( xml = function(doc, txt = readLines(doc), ...) {
                  readXMLScript(xmlParse(txt, asText = TRUE), ...)
  },
  Stangled = getTangledFrags,
  R = readRExpressions,
  JSS = readJSSCode,
  labeled = readAnnotatedScript,
  org = readOrgmode,
  elist = function(doc, txt) txt,
  language = function(doc, txt) list(txt)#,
  ## call = function(doc, txt) as(txt, "expression")
  )





# Functions to get unique names for the tasks.
makeDuplicatedNames =
function(x)
{
 tmp = structure(1:length(x) , names = x)
 vals = tapply(tmp, x, makeTaskNames)
 x[unlist(vals)] = unlist(lapply(vals, names))
 x
}

makeTaskNames =
function(x)
{
  if(length(x) > 1)
     names(x) = trim(sprintf("%s %d", names(x)[1], seq(along = x)))
  x
}

trim =
function(x)
  gsub("^[[:space:]]+|[[:space:]]+$", "", x)

historyAsScript =
function()
{
  f = tempfile()
  on.exit(unlink(f))
  
  utils::savehistory(f)
  lines = readLines(f, encoding = "UTF-8")[-1]
  lines = gsub("\\\\040", " ", lines)
  expr = lapply(lines, function(ll) tryCatch(parse(text = ll), error = function(e) {NULL}))
  new("Script",  .Data = expr[!sapply(expr, is.null)])
}


