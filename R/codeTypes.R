#
# Heuristics for determining the type of a code block/chunk,
# e.g. plot, modeling, reading data, filtering
# We can do some of this by looking at the functions that are called.

GraphicsPackages = c("package:grDevices", "package:graphics", "package:lattice", "package:ggplot2")
ModelingPackages = c("package:stats", "package:lme", "package:lmer", "package:lme4")
ModelingFunctions = c("glm", "lm", "glmm",  "gam")

InputFunctions = c("read.table", "read.csv", "read.csv2", "read.delim", "read.fwf", "file", "url", "load",
                   "gzfile", "bzfile", "download.file", "pipe", "fifo", "unz",
                   "data.frame", "matrix", "readRDS", "readLines")
OutputFunctions = c("save", "save.image", "write", "dput", "dump", "write.table", "write.csv", "saveRDS")
SimulationFunctions = c("sample",
               "rnorm", "rexp", "rcauchy", "rchisq", "rf", "rgamma", "rlnorm", "rlogis", "rstudent", "rt", "runif", "rweibull",
               "rmultinom", "rnbinom", "rgeom", "rhyper", "rpois", "rbinom", "rwilcox", "rsignrank")

FilteringFunctions = c("[", "[[", "[[<-", "&", "|", "<", "<=", ">=", "!=", "!", "tapply", "by", "filter", "subset", "select")
ArithmeticFunctions = c("[", "[[", "[[<-")

PresentationFunctions = c("print", "cat")

InitFunctions = c("library", "require")

guessTaskType =
  #
  # Try to infer the type of task a code block is doing.
  #
  # This is very simple at present, so add many more rules as you think of them.
  # Perhaps we could look at data and label it and build a classifier.
  #
  # We may want to come up with "probabilities" for different task types.
  #
  
function(e, info = getInputs(e))
{
  ## format of info@functions is logical indicating local
  ## with fun names as vec names. See classes.R ~GB
  funs = names(info@functions)
  pkgs = sapply(funs, find)

  ans = character()
  
  if(any(pkgs %in%  GraphicsPackages))
      ans = c(ans, "graphics")
  else if (length(grep("^(geom_|stat_|aes|facet_|scale_|theme_)", funs)))
      ans = c(ans, "graphics")

  if(any(funs %in%  InputFunctions) || any(c("package:RCurl") %in% pkgs))
     ans = c(ans, "data input")
  else if(length(grep("^read", funs)))
     ans = c(ans, "data input")


  if(any(funs %in% OutputFunctions))
     ans = c(ans, "data output")

  if(any(funs %in% SimulationFunctions))
     ans = c(ans, "simulation")

  if(any(pkgs %in%  ModelingPackages))
     ans = c(ans, "modeling")
  if(any(funs %in%  ModelingFunctions))
     ans = c(ans, "modeling")  
  

  if(any(funs %in%  InitFunctions) || length(info@libraries))
     ans = c(ans, "initialization")

  if(any(funs %in%  PresentationFunctions))
     ans = c(ans, "initialization")      

  if(length(ans))
    unique(ans)
  else
    NA
}
