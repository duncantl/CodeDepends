doc = htmlParse("http://penguin.biostat.jhsph.edu/cpkg.html")
unlist(getNodeSet(doc, "//a/@href"))
code = paste("http://penguin.biostat.jhsph.edu/", grep("\\.R$", unlist(getNodeSet(doc, "//a/@href")), val = TRUE), sep = "")
mapply(download.file, code, basename(code))
