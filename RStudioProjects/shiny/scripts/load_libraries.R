# load required libraries
libraries = c("causalbn", "bnlearn", "entropy", "reshape2", "ggplot2", "Rgraphviz", "magic", "expm", 
              "gtools", "readr", "ggplot2", "networkD3", "shiny", "shinydashboard")
lapply(libraries, require, character.only = TRUE)

sourceDir <- function(path, fileName = ".R", trace = TRUE, ...) {
  allFiles = list.files(path, pattern = fileName)
  for (file in allFiles) {
    if(trace) cat(file,":")
    source(file.path(path, file), ...)
    if(trace) cat("\n")
  }
}

# source from local repository
sourceDir("../mbDiscoveryR/randBN/")
sourceDir("../mbDiscoveryR/mbMMLCPT/")
sourceDir("../mbDiscoveryR/learnBN/")
sourceDir("../mbDiscoveryR/mbMMLLogit/")
sourceDir("../mbDiscoveryR/searches/")
sourceDir("source code//")

