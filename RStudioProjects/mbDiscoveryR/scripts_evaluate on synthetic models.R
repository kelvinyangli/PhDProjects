libraries = c("bnlearn", "pcalg", "gRain", "gtools", "entropy", "reshape2", "ggplot2", "Rgraphviz")
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

# apply mbMMLCPT on the alarm network
# each node will be considered as a target and the average of precision and recall over all nodes are reported
# euclidean distance to the perfect precision and recall is also reported, that is, 
# sqrt((1 - precision)^2 + (1 - recall)^2)


# automate testing for mb discovery algorithm
# tested algorithms: iamb (c++), pcbm (c++), mmlcpt (R)
# evaluation measures: precision, recall, distance, f-measure
# details: both iamb and pcmb are written in c++ by jena, they require dataset without headers, but the true dag matrix is required. 
# since all methods will be tested under various sample sizes, we'll generate a large dataset at once. 
# 95% confidence intervals are reported together with the computed statistics

setwd("../")

nNodes = 50
maxNParents = 5
maxArity = 6
beta = 1 # concentration parameter
n = 1000
nIter = 10
model = paste(nNodes, maxNParents, maxArity, beta, sep = "_")

# generate dag, cpts, dagMatrix, and save them into correponding directories
for (i in 1:nIter) {
  
  # create model 
  seed = generateSeed()
  set.seed(seed)
  dag = generateDag(nNodes, maxNParents)
  cpts = generateCPTs(dag, maxArity, beta)
  
  dagMatrix = dag2matrix(dag)
  write.table(dagMatrix, paste0(model, "/dag/", model, "_", seed, ".data.net"), row.names = FALSE, col.names = FALSE) # save dag as matrix
  saveRDS(cpts, paste0(model, "/cpts/", model, "_", seed, ".rds")) # save cpts 
  
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
models = list.files(paste0(model, "/cpts/"))
for (ii in 1:length(datasets)) {
  
  data = read.table(paste0(model, "/data/", datasets[ii]))
  cpts = readRDS(paste0(model, "/cpts/", models[ceiling(ii / nIter)])) # load model to get allNodes for parsePCMB
  colnames(data) = names(cpts)
  
  # prepare for mmlCPT
  dataInfo = getDataInfo(data) 
  mbList = list()
  allNodes = names(cpts)
  
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

computeStats(model, "cpt std", n, nIter = nIter, alpha = 0.05, nDigits = 2)
computeStats(model, "cpt sym", n, nIter = nIter, alpha = 0.05, nDigits = 2)

################################################### evaluate iamb and pcmb results ###################################### 

datasets = list.files(paste0(model, "/data/"), pattern = paste0("_", n, "_"))
models = list.files(paste0(model, "/cpts/"))

setwd("pcmb2/") # set wd to pcmb folder

##### pcmb c++
for (ii in 1:length(datasets)) {
  
  # copy data and dagMatrix from model/ folder to pcmb/ with the same name "synModel.data"
  file.copy(paste0("../", model, "/data/", datasets[ii]), "synModel.data", overwrite = TRUE) 
  file.copy(paste0("../", model, "/dag/", datasets[ceiling(ii / nIter)], ".net"), "synModel.data.net", overwrite = TRUE)
  # notice that since data is copied to the same directory with the same name, they will be replaced by each other
  # also since we don't order datasets, so the order of results is different from order of results in mmlcpt
  # but that's not a problem, since we only consider average, but not individual result
  
  results = system(paste0("kmb4 synModel.data ", n, " ", nNodes, " -1 1.0 1 1 0.01"), intern = TRUE) # call c++ pcmb
  
  output = read.table("output.txt")[, 1] # read output.txt from c++ 
  
  cpts = readRDS(paste0("../", model, "/cpts/", models[ceiling(ii / nIter)])) # load model to get allNodes for parsePCMB
  
  mbList = parsePCMB(output, nNodes, names(cpts)) # parse the output file from c++ to get mb index
  
  saveRDS(mbList, paste0("../", model, "/mb/pcmb/", datasets[ii], ".rds")) # save learned mb as .rds
  
  file.remove("output.txt") # remove output.txt
  
}

##### iamb c++
for (ii in 1:length(datasets)) {
  
  # copy data and dagMatrix from model/ folder to pcmb/ with the same name "synModel.data"
  file.copy(paste0("../", model, "/data/", datasets[ii]), "synModel.data", overwrite = TRUE) 
  file.copy(paste0("../", model, "/dag/", datasets[ceiling(ii / nIter)], ".net"), "synModel.data.net", overwrite = TRUE)
  # notice that since data is copied to the same directory with the same name, they will be replaced by each other
  # also since we don't order datasets, so the order of results is different from order of results in mmlcpt
  # but that's not a problem, since we only consider average, but not individual result
  
  results = system(paste0("kmb4 synModel.data ", n, " ", nNodes, " -1 1.0 1 0 0.01"), intern = TRUE) # call c++ pcmb
  
  output = read.table("output.txt")[, 1] # read output.txt from c++
  
  cpts = readRDS(paste0("../", model, "/cpts/", models[ceiling(ii / nIter)])) # load model to get allNodes for parsePCMB
  
  mbList = parsePCMB(output, nNodes, names(cpts)) # parse the output file from c++ to get mb index
  
  saveRDS(mbList, paste0("../", model, "/mb/iamb/", datasets[ii], ".rds")) # save learned mb as .rds
  
  file.remove("output.txt") # remove output.txt
  
}

setwd("../")

computeStats(model, "pcmb", n, nIter = nIter, alpha = 0.05, nDigits = 2)
computeStats(model, "iamb", n, nIter = nIter, alpha = 0.05, nDigits = 2)

# apply symmetry check for pcmb
results = list.files(paste0(model, "/mb/pcmb/"), pattern = paste0("_", n, "_"))
models = list.files(paste0(model, "/cpts/"))
for (i in 1:length(results)) {
  
  cpts = readRDS(paste0(model, "/cpts/", models[ceiling(ii / nIter)])) # load model to get allNodes for parsePCMB
  mbList = readRDS(paste0(model, "/mb/pcmb/", results[i]))
  mbList = symmetryCheck(names(cpts), mbList)
  
  saveRDS(mbList, paste0(model, "/mb/pcmb sym/", results[i]))
  
}
computeStats(model, "pcmb sym", n, nIter = 10, alpha = 0.05, nDigits = 2)

# apply symmetry check for iamb
results = list.files(paste0(model, "/mb/iamb/"), pattern = paste0("_", n, "_"))
for (i in 1:length(results)) {
  
  cpts = readRDS(paste0(model, "/cpts/", models[ceiling(ii / nIter)])) # load model to get allNodes for parsePCMB
  mbList = readRDS(paste0(model, "/mb/iamb/", results[i]))
  mbList = symmetryCheck(names(cpts), mbList)
  
  saveRDS(mbList, paste0(model, "/mb/iamb sym/", results[i]))
  
}
computeStats(model, "iamb sym", n, nIter = 10, alpha = 0.05, nDigits = 2)

######################################################################################################## 
# compute mb of each node using mmlCPT with 2 stages
mbList = list()
for (i in 1:length(allNodes)) {
  
  targetNode = allNodes[i]
  
  mbLearned = findMB(data, targetNode, dataInfo)
  
  mbList[[i]] = mbLearned
  
  mbTrue = bnlearn::mb(cpts, targetNode)
  
  results = mbAccuracy(mbTrue, mbLearned, targetNode, allNodes)
  
  resultsMatrix[i, "precision"] = results$precision
  resultsMatrix[i, "recall"] = results$recall
  resultsMatrix[i, "distance"] = sqrt((1 - results$precision) ^ 2 + (1 - results$recall) ^ 2)
  
} 

colMeans(resultsMatrix)

# use symmetry condition to re-check for mb candidate for each node
names(mbList) = allNodes
for (i in 1:length(allNodes)) {
  
  node = allNodes[i] 
  
  # if node x is in mb(y), then y is also in mb(x)
  for (j in 1:length(allNodes)) {
    
    if ((j != i) && (node %in% mbList[[j]])) {# if node exists in the mb of another node
      
      if (!allNodes[j] %in% mbList[[i]]) {# if this other node is not in mb(node)
        
        # then add it into mb(node)
        mbList[[i]] = c(mbList[[i]], allNodes[j])
        
      } # end if
      
    } # end if
    
  } # end for j
  
} # end for i


# evaluation
for (i in 1:length(allNodes)) {
  
  targetNode = allNodes[i]
  
  mbLearned = mbList[[i]]
  mbTrue = bnlearn::mb(cpts, targetNode)
  
  results = mbAccuracy(mbTrue, mbLearned, targetNode, allNodes)
  
  resultsMatrix[i, "precision"] = results$precision
  resultsMatrix[i, "recall"] = results$recall
  resultsMatrix[i, "distance"] = sqrt((1 - results$precision) ^ 2 + (1 - results$recall) ^ 2)
  
}
colMeans(resultsMatrix)

