#setwd("C:/PhDProjects/RStudioProjects/mbDiscoveryR/")

#libraries = c("bnlearn", "readr", "foreign", "pcalg", "gRain", "gtools", "entropy", "reshape2", "ggplot2", "Rgraphviz", "arm")
libraries = c("bnlearn", "entropy", "reshape2", "ggplot2", "Rgraphviz", "gRain", "magic", "expm",
              "gtools", "readr", "networkD3", "devtools", "lglbnlearn", "parallel", "foreach", "doParallel")
lapply(libraries, require, character.only = TRUE)

#library(bnlearn)
#library(gtools)
#options(scipen = 10)
# sourceDir <- function(path, fileName = ".R", trace = TRUE, ...) {
#   allFiles = list.files(path, pattern = fileName)
#   for (file in allFiles) {
#     if(trace) cat(file,":")
#     source(file.path(path, file), ...)
#     if(trace) cat("\n")
#   }
# }

# source from local repository
#sourceDir("/home/kl/Documents/PhDProjects/RStudioProjects/mbDiscoveryR/mbMMLCPT/")
#sourceDir("/home/kl/Documents/PhDProjects/RStudioProjects/mbDiscoveryR/randBN/")
#sourceDir("/home/kl/Documents/PhDProjects/RStudioProjects/mbDiscoveryR/learnBN/")
#sourceDir("testing/")
#sourceDir("/home/kl/Documents/PhDProjects/RStudioProjects/mbDiscoveryR/mbMMLLogit/")
#sourceDir("/home/kl/Documents/PhDProjects/RStudioProjects/mbDiscoveryR/searches/")
#sourceDir("/home/kl/Documents/PhDProjects/RStudioProjects/local2global/source code/")


# install.packages("../lglbnlearn_0.0.0.9000.tar.gz",repos=NULL, type = "source")
