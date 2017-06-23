#setwd("C:/PhDProjects/RStudioProjects/mbDiscoveryR/")

#libraries = c("bnlearn", "readr", "foreign", "pcalg", "gRain", "gtools", "entropy", "reshape2", "ggplot2", "Rgraphviz", "arm")
libraries = c("causalbn", "bnlearn", "entropy", "reshape2", "ggplot2", "Rgraphviz", "magic", "expm", "gtools", "readr", "ggplot2", "networkD3")
lapply(libraries, require, character.only = TRUE)

#library(bnlearn)
#library(gtools)
#options(scipen = 10)
sourceDir <- function(path, fileName = ".R", trace = TRUE, ...) {
  allFiles = list.files(path, pattern = fileName)
  for (file in allFiles) {
    if(trace) cat(file,":")
    source(file.path(path, file), ...)
    if(trace) cat("\n")
  }
}

# source from local repository
sourceDir("~/PhDProjects/RStudioProjects/mbDiscoveryR/mbMMLCPT/")
sourceDir("~/PhDProjects/RStudioProjects/mbDiscoveryR/randBN/")
sourceDir("~/PhDProjects/RStudioProjects/mbDiscoveryR/learnBN/")
#sourceDir("testing/")
sourceDir("~/PhDProjects/RStudioProjects/mbDiscoveryR/mbMMLLogit/")
sourceDir("~/PhDProjects/RStudioProjects/mbDiscoveryR/searches/")
sourceDir("~/PhDProjects/RStudioProjects/local2global/source code/")

# log factorial sheet
logFactorialSheet = read.csv("~/PhDProjects/RStudioProjects/local2global/logFactorial_1to10000.csv") 

