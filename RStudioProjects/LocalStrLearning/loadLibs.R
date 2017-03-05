#setwd("C:/PhDProjects/RStudioProjects/mbDiscoveryR/")

#libraries = c("bnlearn", "readr", "foreign", "pcalg", "gRain", "gtools", "entropy", "reshape2", "ggplot2", "Rgraphviz", "arm")
libraries = c("causalbn", "bnlearn", "entropy", "reshape2", "ggplot2", "Rgraphviz", "magic", "expm")
lapply(libraries, require, character.only = TRUE)

#library(bnlearn)
#library(gtools)
#options(scipen = 10)
#sourceDir <- function(path, fileName = ".R", trace = TRUE, ...) {
#  allFiles = list.files(path, pattern = fileName)
#  for (file in allFiles) {
#    if(trace) cat(file,":")
#    source(file.path(path, file), ...)
#    if(trace) cat("\n")
#  }
#}

# source from local repository
#sourceDir("../mbDiscoveryR/mbMMLCPT/")
#sourceDir("../mbDiscoveryR/randBN/")
#sourceDir("../mbDiscoveryR/learnBN/")
#sourceDir("testing/")
#sourceDir("../mbDiscoveryR/mbMMLLogit/")
#sourceDir("../mbDiscoveryR/searches/")
#sourceDir("source code//")


