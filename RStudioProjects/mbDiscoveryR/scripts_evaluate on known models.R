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

# apply mbMMLCPT on the alarm network
# each node will be considered as a target and the average of precision and recall over all nodes are reported
# euclidean distance to the perfect precision and recall is also reported, that is, 
# sqrt((1 - precision)^2 + (1 - recall)^2)


##############################################################################################################
# re-sample cpts for alarm using uniform prior i.e dirichlet beta = 1
setwd("../")
dag = readRDS("Known BNs/barleyDag.rds") # read dag
arities = readRDS("Known BNs/barleyArity.rds") # read arities
n = 150
beta = 1
nIter = 10

res.std = matrix(0, nrow = nIter ^ 2, ncol = 4)
res.sym = res.std

for (ii in 1:nIter) {
  
  seed1 = generateSeed()
  set.seed(seed1)
  
  cpts = generateCPTs2(dag, arities, beta) # sample cpts using specified arities
  
  for (jj in 1:nIter) {
    
    seed2 = generateSeed()
    set.seed(seed2)
    
    data = rbn(cpts, n) # sample data
    write.table(data, paste0("barley data/barley_", n, "_", seed1, "_", seed2, ".data"), row.names = FALSE, col.names = FALSE) # save data
    
    dataInfo = getDataInfo(data) 
    mbList = list()
    allNodes = colnames(data)
    resultsMatrix = matrix(0, nrow = length(allNodes), ncol = 4, dimnames = list(allNodes, c("precision", "recall", "distance", "fmeasure")))
    
    # compute mb of each node using standard forward selection
    for (i in 1:length(allNodes)) {
      
      targetNode = allNodes[i]
      
      mbLearned = mbForwardSelection.fast(data, targetNode, mmlCPT.fast, dataInfo$arities, dataInfo$indexListPerNodePerValue)
      
      mbList[[i]] = mbLearned
      
      mbTrue = bnlearn::mb(dag, targetNode)

      results = mbAccuracy(mbTrue, mbLearned, targetNode, allNodes)
      
      resultsMatrix[i, "precision"] = results$precision
      resultsMatrix[i, "recall"] = results$recall
      resultsMatrix[i, "distance"] = sqrt((1 - results$precision) ^ 2 + (1 - results$recall) ^ 2)
      if (results$precision + results$recall == 0) {
        
        resultsMatrix[i, "fmeasure"] = 0
        
      } else {
        
        resultsMatrix[i, "fmeasure"] = 2 * results$precision * results$recall / (results$precision + results$recall)
        
      } # end else 
      
    } # end for i 
    
    names(mbList) = allNodes
    saveRDS(mbList, paste0("barley mb/cpt std/", n, "_", seed1, "_", seed2, ".rds")) # save learned mb as .rds
    res.std[(ii - 1) * nIter + jj, ] = colMeans(resultsMatrix)
    
    # use symmetry condition to re-check for mb candidate for each node
    
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
    
    saveRDS(mbList, paste0("barley mb/cpt sym/", n, "_", seed1, "_", seed2, ".rds")) # save learned mb as .rds
    
    # evaluation
    for (i in 1:length(allNodes)) {
      
      targetNode = allNodes[i]
      
      mbLearned = mbList[[i]]
      mbTrue = bnlearn::mb(dag, targetNode)
      
      results = mbAccuracy(mbTrue, mbLearned, targetNode, allNodes)
      
      resultsMatrix[i, "precision"] = results$precision
      resultsMatrix[i, "recall"] = results$recall
      resultsMatrix[i, "distance"] = sqrt((1 - results$precision) ^ 2 + (1 - results$recall) ^ 2)
      if (results$precision + results$recall == 0) {
        
        resultsMatrix[i, "fmeasure"] = 0
        
      } else {
        
        resultsMatrix[i, "fmeasure"] = 2 * results$precision * results$recall / (results$precision + results$recall)
        
      } # end else
      
    }
    res.sym[(ii - 1) * nIter + jj, ] = colMeans(resultsMatrix)
    
  } # end for jj
  
} # end for ii 
round(computeCI(res.std), 2) # compute mean and CI
round(computeCI(res.sym), 2) # compute mean and CI
write.csv(res.std, paste0("results/barley_cpt_std_", n, ".csv"), row.names = FALSE)
write.csv(res.sym, paste0("results/barley_cpt_sym_", n, ".csv"), row.names = FALSE)

##############################################################################################################
setwd("pcmb/") # set wd to pcmb folder

##### pcmb from c++
res.std = matrix(0, nrow = nIter ^ 2, ncol = 4)
datasets = list.files("../barley data/", pattern = paste0("barley_", n, "_"))
allNodes = bnlearn::nodes(dag)
for (ii in 1:length(datasets)) {
  
  file.copy(paste0("../barley data/", datasets[ii]), "barley.data", overwrite = TRUE) # copy data from "alarm data" to "pcmb" with new name "alarm.data"
  # notice that since data is copied to the same directory with the same name, they will be replaced by each other
  # also since we don't order datasets in "alarm data" folder, so the order of results is different from order of results in mmlcpt
  # but that's not a problem, since we only consider average, but not individual result
  
  results = system(paste0("kmb4 barley.data ", n, " 48 -1 1.0 1 1 0.01"), intern = TRUE)
  
  mbList = getPCMBsyn(results, 1) # convert c++ output of pcmb to list in R
  names(mbList) = allNodes
  saveRDS(mbList, paste0("../barley mb/iamb/", datasets[ii], ".rds")) # save learned mb as .rds
  
  resultsMatrix = matrix(0, nrow = length(allNodes), ncol = 4, dimnames = list(allNodes, c("precision", "recall", "distance", "fmeasure")))
  
  for (i in 1:length(mbList)) {
    
    targetNode = allNodes[i]
    
    mbLearned = allNodes[mbList[[i]]]
    mbTrue = bnlearn::mb(dag, targetNode)
    
    results = mbAccuracy(mbTrue, mbLearned, targetNode, allNodes)
    
    resultsMatrix[i, "precision"] = results$precision
    resultsMatrix[i, "recall"] = results$recall
    resultsMatrix[i, "distance"] = sqrt((1 - results$precision) ^ 2 + (1 - results$recall) ^ 2)
    if (results$precision + results$recall == 0) {
      
      resultsMatrix[i, "fmeasure"] = 0
      
    } else {
      
      resultsMatrix[i, "fmeasure"] = 2 * results$precision * results$recall / (results$precision + results$recall)
      
    } # end else 
    
  } # end for i
  
  res.std[ii, ] = colMeans(resultsMatrix) # compute average statistics for each dataset
  
}
round(computeCI(res.std), 2) # compute mean and CI
write.csv(res.std, paste0("../results/barley_pcmb_", n, ".csv"), row.names = FALSE)


##### iamb from c++
res.std = matrix(0, nrow = nIter ^ 2, ncol = 4)
for (ii in 1:length(datasets)) {
  
  file.copy(paste0("../barley data/", datasets[ii]), "barley.data", overwrite = TRUE) # copy data from "alarm data" to "pcmb" with new name "alarm.data"
  # notice that since data is copied to the same directory with the same name, they will be replaced by each other
  # also since we don't order datasets in "alarm data" folder, so the order of results is different from order of results in mmlcpt
  # but that's not a problem, since we only consider average, but not individual result
  
  results = system(paste0("kmb4 barley.data ", n, " 48 -1 1.0 1 0 0.01"), intern = TRUE)
  
  mbList = getPCMBsyn(results, 0) # convert c++ output of iamb to list in R
  names(mbList) = allNodes
  saveRDS(mbList, paste0("../barley mb/iamb/", datasets[ii], ".rds")) # save learned mb as .rds
  
  resultsMatrix = matrix(0, nrow = length(allNodes), ncol = 4, dimnames = list(allNodes, c("precision", "recall", "distance", "fmeasure")))
  
  for (i in 1:length(mbList)) {
    
    targetNode = allNodes[i]
    
    mbLearned = allNodes[mbList[[i]]]
    mbTrue = bnlearn::mb(dag, targetNode)
    
    results = mbAccuracy(mbTrue, mbLearned, targetNode, allNodes)
    
    resultsMatrix[i, "precision"] = results$precision
    resultsMatrix[i, "recall"] = results$recall
    resultsMatrix[i, "distance"] = sqrt((1 - results$precision) ^ 2 + (1 - results$recall) ^ 2)
    if (results$precision + results$recall == 0) {
      
      resultsMatrix[i, "fmeasure"] = 0
      
    } else {
      
      resultsMatrix[i, "fmeasure"] = 2 * results$precision * results$recall / (results$precision + results$recall)
      
    } # end else 
    
  } # end for i
  
  res.std[ii, ] = colMeans(resultsMatrix) # compute average statistics for each dataset
  
}
round(computeCI(res.std), 2) # compute mean and CI
write.csv(res.std, paste0("../results/barley_iamb_", n, ".csv"), row.names = FALSE)




