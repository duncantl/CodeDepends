readJSSCode =
function(doc, txt = readLines(doc), codeRegExp = "RCode", ...)
{
  txt = expandInputs(txt, doc, ...)
  
  codeRegExp = paste(codeRegExp, collapse = "|")
  rx = sprintf("\\\\%s\\{(%s)\\}", c("begin", "end"), rep(codeRegExp, 2))

  start = grep(rx[1], txt)
  end = grep(rx[2], txt)

  if(length(start) != length(end)) 
     stop("mismatched begin and end code environments")

  code = mapply(function(i, j) {
                   parse(text = txt[ seq(i+1L,  j-1L) ])
                }, start, end)
#  new("Script", code)
}


expandInputs =
  #
  #
  #  deal with \endinput
  #
function(txt, baseURL, recursive = TRUE, verbose = FALSE)
{
  i = grep("\\\\input\\{.*}", txt)
  if(length(i)) {

    ans = lapply(txt, function(x) x)

    ins = gsub("\\\\input\\{(.*)}.*", "\\1", txt[i])
    w = grepl("\\.tex$", ins)
    ins[!w] = sprintf("%s.tex", ins[!w])
    ins = sapply(ins, getRelativeURL, baseURL)
    ans[i] = lapply(ins, texInput, recursive = recursive, verbose = verbose)

    txt = unlist(ans)
  }

  txt
}

texInput =
function(filename, recursive = TRUE, verbose = FALSE)
{
  if(verbose)
    cat("processing", filename, "\n")
  
  txt = readLines(filename)
  i = grep("\\\\endinput", txt)
  if(length(i)) {
      # check not commented out. Could do this in one regexp.
    i = grep("%.*\\\\endinput", invert = TRUE)
    if(length(i))
      txt = txt[1:(i[1] - 1)]
  }

  if(recursive)
    txt = expandInputs(txt, filename, TRUE,  verbose = verbose)
  
  txt
}
