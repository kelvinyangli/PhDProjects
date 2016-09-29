#libraries = c("bnlearn", "pcalg", "gRain", "gtools", "entropy", "reshape2", "ggplot2", "Rgraphviz")
#lapply(libraries, require, character.only = TRUE)

library(bnlearn)
library(gtools)
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
sourceDir("mbMMLLogit/")

# apply mbMMLCPT on the alarm network
# each node will be considered as a target and the average of precision and recall over all nodes are reported
# euclidean distance to the perfect precision and recall is also reported, that is, 
# sqrt((1 - precision)^2 + (1 - recall)^2)


##############################################################################################################
# re-sample cpts for alarm using uniform prior i.e dirichlet beta = 1
setwd("../")

model = "alarm"
dag = readRDS(paste0("Known BNs/", model, "Dag.rds")) # read dag
arities = readRDS(paste0("Known BNs/", model, "Arity.rds")) # read arities
n = 500
beta = 1
nIter = 10

allNodes = bnlearn::nodes(dag)

# generate cpts with specified arities and concentration parameter 
for (i in 1:nIter) {
  
  # create model 
  seed = generateSeed()
  set.seed(seed)
  cpts = generateCPTs2(dag, arities, beta) # sample cpts using specified arities
  
  saveRDS(cpts, paste0(model, " nonUniform/cpts/", "alarm_", seed, ".rds")) # save cpts 
  
  Sys.sleep(0.01) # suspend execution for 0.01 seconds to avoid generating the same seed
  
} # end for i


# generate data 
models = list.files(paste0(model, "/cpts/"))
for (i in 1:length(models)) {
  
  cpts = readRDS(paste0(model, "/cpts/", models[i]))
  
  for (j in 1:nIter) { 
    
    # sample data
    seed = generateSeed()
    set.seed(seed)
    data = rbn(cpts, n)
    
    fileName = paste(strsplit(models[i], ".rds")[[1]], n, seed, sep = "_")
    write.table(data, paste0(model, "/data/", fileName, ".data"), row.names = FALSE, col.names = FALSE) # save data
    
    Sys.sleep(0.01) # suspend execution for 0.01 seconds to avoid generating the same seed
    
  } # end for j
  
} # end for i


# apply mmlCPT 
datasets = list.files(paste0(model, "/data/"), pattern = paste0("_", n, "_"))
for (ii in 1:length(datasets)) {
  
  data = read.table(paste0(model, "/data/", datasets[ii]))
  colnames(data) = allNodes
  
  # prepare for mmlCPT
  dataInfo = getDataInfo(data) 
  mbList = list()

  # compute mb of each node using standard forward selection
  for (i in 1:length(allNodes)) {
    
    targetNode = allNodes[i]
    mbList[[i]] = mbForwardSelection.fast(data, targetNode, mmlCPT.fast, dataInfo$arities, dataInfo$indexListPerNodePerValue)
    
  } # end for i 
  
  saveRDS(mbList, paste0(model, "/mb/cpt std/", datasets[ii], ".rds")) # save mbList into folder
  
  # use symmetry condition to re-check for mb candidate for each node
  mbList = symmetryCheck(allNodes, mbList)
  
  saveRDS(mbList, paste0(model, "/mb/cpt sym/", datasets[ii], ".rds")) # save mbList into folder
  
} # end for ii

computeStats2(model, "cpt std", n, nIter = nIter, alpha = 0.05, nDigits = 2)
computeStats2(model, "cpt sym", n, nIter = nIter, alpha = 0.05, nDigits = 2)


# apply mmlLogit
datasets = list.files(paste0(model, "/data/"), pattern = paste0("_", n, "_"))
for (ii in 1:length(datasets)) {
  
  data = read.table(paste0(model, "/data/", datasets[ii]))
  colnames(data) = allNodes
  
  # prepare for mmlLogit
  dataInfo = getDataInfo(data) 
  indicatorMatrix = getIndicator(data)
  mbList = list()
  
  # compute mb of each node using standard forward selection
  for (i in 1:length(allNodes)) {
    
    targetNode = allNodes[i]
    mbList[[i]] = mbForwardSelection(data, targetNode, mmlLogit, dataInfo$arities, dataInfo$indexListPerNodePerValue, 
                                     base = exp(1), indicatorMatrix, mbSize = 1000, interaction = F, debug = F)
    
  } # end for i 
  
  saveRDS(mbList, paste0(model, "/mb/logit1st/", datasets[ii], ".rds")) # save mbList into folder
  
  # use symmetry condition to re-check for mb candidate for each node
  mbList = symmetryCheck(allNodes, mbList)
  
  saveRDS(mbList, paste0(model, "/mb/logit1st sym/", datasets[ii], ".rds")) # save mbList into folder
  
} # end for ii

##############################################################################################################

datasets = list.files(paste0(model, "/data/"), pattern = paste0("_", n, "_"))
allNodes = bnlearn::nodes(dag)

setwd("pcmb/") # set wd to pcmb folder

file.remove("output.txt")

##### pcmb from c++
for (ii in 1:length(datasets)) {
  
  file.copy(paste0("../", model, "/data/", datasets[ii]), paste0(model, ".data"), overwrite = TRUE) # copy data from "alarm data" to "pcmb" with new name "alarm.data"
  # notice that since data is copied to the same directory with the same name, they will be replaced by each other
  # also since we don't order datasets in "alarm data" folder, so the order of results is different from order of results in mmlcpt
  # but that's not a problem, since we only consider average, but not individual result
  
  results = system(paste0("kmb4 ", model, ".data ", n, " ", length(allNodes), " -1 1.0 1 1 0.01"), intern = TRUE)
  
  output = read.table("output.txt")[, 1] # load output file from c++
  
  mbList = parsePCMB(output, length(allNodes), allNodes)
  
  saveRDS(mbList, paste0("../", model, "/mb/pcmb/", datasets[ii], ".rds")) # save learned mb as .rds
  
  file.remove("output.txt")
  
}

##### iamb from c++
for (ii in 1:length(datasets)) {
  
  file.copy(paste0("../", model, "/data/", datasets[ii]), paste0(model, ".data"), overwrite = TRUE) # copy data from "alarm data" to "pcmb" with new name "alarm.data"
  # notice that since data is copied to the same directory with the same name, they will be replaced by each other
  # also since we don't order datasets in "alarm data" folder, so the order of results is different from order of results in mmlcpt
  # but that's not a problem, since we only consider average, but not individual result
  
  results = system(paste0("kmb4 ", model, ".data ", n, " ", length(allNodes), " -1 1.0 1 0 0.01"), intern = TRUE)
  
  output = read.table("output.txt")[, 1] # load output file from c++
  
  mbList = parsePCMB(output, length(allNodes), allNodes)
  
  saveRDS(mbList, paste0("../", model, "/mb/iamb/", datasets[ii], ".rds")) # save learned mb as .rds
  
  file.remove("output.txt")
  
}

setwd("../")
computeStats2(model, "pcmb", n, nIter = 10, alpha = 0.05, nDigits = 2)
computeStats2(model, "iamb", n, nIter = 10, alpha = 0.05, nDigits = 2)

# apply symmetry check for pcmb
results = list.files(paste0(model, "/mb/pcmb/"), pattern = paste0("_", n, "_"))
for (i in 1:length(results)) {
  
  mbList = readRDS(paste0(model, "/mb/pcmb/", results[i]))
  mbList = symmetryCheck(allNodes, mbList)
  
  saveRDS(mbList, paste0(model, "/mb/pcmb sym/", results[i]))
  
}
computeStats2(model, "pcmb sym", n, nIter = 10, alpha = 0.05, nDigits = 2)

# apply symmetry check for iamb
results = list.files(paste0(model, "/mb/iamb/"), pattern = paste0("_", n, "_"))
for (i in 1:length(results)) {
  
  mbList = readRDS(paste0(model, "/mb/iamb/", results[i]))
  mbList = symmetryCheck(allNodes, mbList)
  
  saveRDS(mbList, paste0(model, "/mb/iamb sym/", results[i]))
  
}
computeStats2(model, "iamb sym", n, nIter = 10, alpha = 0.05, nDigits = 2)






