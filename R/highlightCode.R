getFunctionLinks = 
function(funcNames)
{
  pkg = gsub("package:", "", sapply(funcNames, function(x) find(x)[1]))
  sprintf("%s/%s.html", pkg, funcNames)
}

htmlRenderer =
function(addFunctionLinks = TRUE,  checkURLs = TRUE, h = renderer_html())
{

  if(is.logical(addFunctionLinks)) {
    addFunctionLinks = if(addFunctionLinks) getFunctionLinks else NULL
  } else if(is.character(addFunctionLinks))  {

  }
  
  symbols = list()
  h$formatter =
    function (tokens, styles, ...) {

      ans = ifelse(styles == "", tokens,
                       sprintf("<span class=\"%s\">%s</span>", styles, tokens))

      if(!is.null(addFunctionLinks)) {
           # make this more general to allow caller specify how to construct the URL. Use a function
           # but allow a regular expression.
        w = styles == "functioncall"

        ans[w] = sprintf("<a href='%s'>%s</a>",
                           getFunctionLinks(tokens[w]), tokens[w], ans[w])
     }

      w = which(styles == "symbol")
      p = tokens[ w - 2] %in% c("require", "library")
      if(any(p)) 
        ans[w][p]  = sprintf("<a href='http://www.omegahat.org/%s'>%s</a>", tokens[w][p], ans[w][p])

      if(any(!p)) {
        i = w[!p]
        ans[i]  = sprintf('<span id="sym%d" class="%s" onmouseover=\'showVariable("%s", symIdentifiers);\' onmouseout=\'hideVariable("%s", symIdentifiers);\'>%s</span>',
                      seq(along = i), styles[i], tokens[i], tokens[i], tokens[i])


        symbols <<- split(sprintf("sym%d", seq(along = i)), tokens[i])
      }


      w = which(styles == "string")
      tmp = gsub('(^"|"$)', "", tokens[w])
      e = file.exists(tmp)
      if(checkURLs) {
        library(RCurl)
        e[!e] = url.exists(tmp[!e])
      }
      i = w[e]
      if(length(i)) {
        ans[i]  = sprintf('<a href="%s">%s</a>',  sapply(tmp[e], path.expand), ans[i])
      }
 
      ans
    }

    h$symbols = function() symbols
    h
}


highlightCode =
function(obj, out = NULL, addFunctionLinks = TRUE, inline = TRUE, h = htmlRenderer(addFunctionLinks),
         css = system.file("CSS", "highlight.css", package = "CodeDepends"),
         jsCode = system.file("JavaScript", "highlightSymbols.js", package = "CodeDepends"))
{
  library(highlight)

  html = if(is.character(obj))
           highlight(obj, NULL, renderer = h)
         else {
           stop("not implemented yet") #XXXX
           highlight(parse.output = obj, NULL, renderer = h)
         }

  library(XML)
  library(RJSONIO)

  doc = htmlParse(html, asText = TRUE)

  pre = getNodeSet(doc, "//pre")[[1]]
  xmlAttrs(pre) = c(class = "Rcode")


    # add the CSS as a 
  head = xmlRoot(doc)[["head"]]
  node = getNodeSet(doc, "//head/style")[[1]]
  removeNodes(node)


  if(FALSE && inline)
     newXMLNode("link", attrs = c(rel = "stylesheet", type = "text/css"),
                paste(c("", readLines(css), ""), collapse = "\n"), parent = head)    
  else
     newXMLNode("link", attrs = c(rel = "stylesheet", type = "text/css", href = css),
                parent = head)

browser()
  

  if(inline)
    newXMLNode("script", attrs = c(type = "text/javascript"),
                newXMLCommentNode(paste(c("", readLines(jsCode), ""), collapse = "\n")),
                parent = head)    
  else
    newXMLNode("script", attrs = c(type = "text/javascript", src = jsCode),
                parent = head)

    # inline the identifiers for the different variables/symbols.
  newXMLNode("script", attrs = c(type = "text/javascript"),
            sprintf("var symIdentifiers = %s;", toJSON(h$symbols())), parent = head)


   # clean up extra new lines.
  tt = getNodeSet(doc, "//body//pre/text()")
  sapply(tt, function(x) xmlValue(x) = gsub("\\\n{2}", "\\\n", xmlValue(x)))


  if(length(out) && !is.na(out))
    saveXML(doc, out)
  else
    doc
}



