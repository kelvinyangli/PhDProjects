# this is a script for running mb discovery using mmlCPT
# to run this script, please download all three folders from google drive: createBN, mbMMLCPT, testing
# and put them under the same folder as this simple script 
# then simply run each line in this simple script, and you will get the results of a Markov blanket using mmlCPT

libraries = c("bnlearn", "gRain", "gtools", "entropy", "reshape2", "ggplot2", "Rgraphviz")
lapply(libraries, require, character.only = TRUE)

sourceDir <- function(path, fileName = NULL, trace = TRUE, ...) {
  allFiles = list.files(path, pattern = fileName)
  for (file in allFiles) {
    if(trace) cat(file,":")
    source(file.path(path, file), ...)
    if(trace) cat("\n")
  }
}

# source from local repository
sourceDir("mbMMLCPT/")
sourceDir("createBN/")
sourceDir("testing/")

dag = generateDag(12, 3) # create a dag with 12 nodes and maximum 3 parents
cpts = generateCPTs(dag, 4, 1) # sample probability distribution for the above dag with maximum arity 4 and uniform prior (beta = 1 in Dirichlet)
data = rbn(cpts, 1000) # sample 1000 instances from the above distribution

graphviz.plot(dag) # visulize dag
# if graphviz.plot doesn't work, try plot(dag)

# find mb 
dataInfo = getDataInfo(data) # get details of data, such as arity 

# use mmlCPT to find mb of a node
mbForwardSelection.fast(data = data, node = "V1", score = mmlCPT.fast, arities = dataInfo$arities, 
                        indexListPerNodePerValue = dataInfo$indexListPerNodePerValue, base = 2, 
                        indicatorMatrix = NULL, interactData = NULL, completeIndicatorMatrix = NULL, debug = FALSE)

bnlearn::mb(dag, "V1") # find the true mb from dag

