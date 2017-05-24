## copied from XML package so we can pass check with no NOTES.
.limitXPathToSection = function(sections, xpath) {
    if (length(section) == 0) 
        return(paste(xpath, collapse = "|"))
    if (is.character(section)) 
        section = paste("@id=", sQuote(section), sep = "")
    paste(outer(section, xpath,
                function(sect, xp) paste("//section[", sect, "]", xp,
                                         sep = "")),
          collapse = "|")
}

getXMLFrags =
  #
  # Read the XML file if necessary, and then extract the elements corresponding to xpath
  # which are the code nodes of interest.
  #
  #
  # This is a basic version of xmlSource().
  #
  #  Why not use
  #    xmlSource(doc, eval = FALSE)
  #
  #
function(doc, sections = character(),  xpath = c("//r:code", "//r:func", "//r:plot", "//r:expr"), asXML = FALSE)
{
 if(is.character(doc))
   doc = xmlInternalTreeParse(doc)

    # merge section with xpath. See xmlInternalSource.R
## xpath = XML:::limitXPathToSection(sections, xpath)  
 xpath = .limitXPathToSection(sections, xpath)
 if(asXML)
   getNodeSet(doc, xpath)
 else {
   old = options()
   on.exit(options(old))
   options(keep.source = FALSE)   
   xpathApply(doc, xpath, parseFrag, FALSE)
 }
}


xmlMakePlot =
  #
  # top-level function used to request the creation of a plot identified by an id
  #
  # makePlot(c("fig", "hist"), "eg.xml")
  #
function(plotIds, doc, sections = character(), env = globalenv())
{
  nodes = readScript(doc, type = "xml")
  frags = lapply(nodes, parseFrag)
  if(is.character(plotIds))
    w = which(sapply(nodes, function(x) xmlName(x, TRUE) == "r:plot" && xmlGetAttr(x, "id", "") %in% plotIds))
  else {
    w = as.integer(plotIds)
    if(!all(sapply(nodes[i], xmlName) == "plot"))
       stop("non-plot node")        
  }

  i = unique(unlist(lapply(w, getSectionDepends, frags, index = TRUE)))
  invisible(sapply(sort(i), function(index) {
                             eval(frags[[index]], env)
                           }))
}



xmlPlotIds =
function(doc)
{
  if(is.character(doc))
    doc = xmlInternalTreeParse(doc)

  xpathSApply(doc, "//r:plot", xmlGetAttr, "id")
}
