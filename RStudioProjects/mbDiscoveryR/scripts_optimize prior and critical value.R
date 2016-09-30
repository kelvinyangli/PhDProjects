# scripts for training optimal prior for mml and critical value for ci-based methods, i.e pcmb and iamb
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
#################################################################################################
# training for optimal prior/critical value
setwd("C:/mbDiscoveryExperimentalResults/")
model = "alarm"
dag = readRDS(paste0("Known BNs/", model, "Dag.rds")) # read dag
arities = readRDS(paste0("Known BNs/", model, "Arity.rds"))
n = 500
datasets = list.files(paste0(model, "/data training/"), pattern = paste0("_", n, "_"))
allNodes = bnlearn::nodes(dag)

# optimize mml prior
conPars = 0.3
for (j in 1:length(conPars)) {
  
  dir.create(paste0("alarm/mb/", "cpt std ", conPars[j]))

  for (ii in 28:length(datasets)) {
    
    data = read.table(paste0(model, "/data training/", datasets[ii]))
    colnames(data) = allNodes
    
    # prepare for mmlCPT
    dataInfo = getDataInfo(data) 
    #dataInfo$arities = arities
    mbList = list()
    
    # compute mb of each node using standard forward selection
    for (i in 1:length(allNodes)) {
      
      targetNode = allNodes[i]
      mbList[[i]] = mbForwardSelectionUsingMMLMultinomialDirichlet(data, targetNode, dataInfo$arities, dataInfo$indexListPerNodePerValue,
                                                                   conPar = conPars[j], base = exp(1), debug = F)
      
    } # end for i 
    
    saveRDS(mbList, paste0(model, "/mb/cpt std ", conPars[j], "/", datasets[ii], ".rds")) # save mbList into folder
  
  }
  
}

# optimize critical value for pcmb
alpha = c(0.03, 0.05, 0.07, 0.09)
#res = c()
setwd("pcmb/")
for (i in 1:length(alpha)) {
  
  #dir.create(paste0("../alarm/mb/", "pcmb ", alpha[i]))
  #setwd("pcmb/")
  file.remove("output.txt")
  
  ##### pcmb from c++
  for (ii in 1:length(datasets)) {
    
    file.copy(paste0("../", model, "/data training/", datasets[ii]), paste0(model, ".data"), overwrite = TRUE) # copy data from "alarm data" to "pcmb" with new name "alarm.data"
    # notice that since data is copied to the same directory with the same name, they will be replaced by each other
    # also since we don't order datasets in "alarm data" folder, so the order of results is different from order of results in mmlcpt
    # but that's not a problem, since we only consider average, but not individual result
    
    results = system(paste0("kmb4 ", model, ".data ", n, " ", length(allNodes), " -1 1.0 1 1 ", alpha[i]), intern = TRUE)
    
    output = read.table("output.txt")[, 1] # load output file from c++
    
    mbList = parsePCMB(output, length(allNodes), allNodes)
    
    saveRDS(mbList, paste0("../", model, "/mb/pcmb ", alpha[i], "/", datasets[ii], ".rds")) # save learned mb as .rds
    
    file.remove("output.txt")
    
  }
  
}

# optimize critical value for iamb
alpha = c(0.0005, 0.0007, 0.0009, 0.05)
#res = c()
setwd("pcmb/")
for (i in 1:length(alpha)) {
  
  dir.create(paste0("../alarm/mb/", "iamb ", alpha[i]))
  #setwd("pcmb/")
  file.remove("output.txt")
  
  ##### pcmb from c++
  for (ii in 1:length(datasets)) {
    
    file.copy(paste0("../", model, "/data training/", datasets[ii]), paste0(model, ".data"), overwrite = TRUE) # copy data from "alarm data" to "pcmb" with new name "alarm.data"
    # notice that since data is copied to the same directory with the same name, they will be replaced by each other
    # also since we don't order datasets in "alarm data" folder, so the order of results is different from order of results in mmlcpt
    # but that's not a problem, since we only consider average, but not individual result
    
    results = system(paste0("kmb4 ", model, ".data ", n, " ", length(allNodes), " -1 1.0 1 0 ", alpha[i]), intern = TRUE)
    
    output = read.table("output.txt")[, 1] # load output file from c++
    
    mbList = parsePCMB(output, length(allNodes), allNodes)
    
    saveRDS(mbList, paste0("../", model, "/mb/iamb ", alpha[i], "/", datasets[ii], ".rds")) # save learned mb as .rds
    
    file.remove("output.txt")
    
  }
  
}


# check accuracy
computeStats2(model, "cpt std 0.3", n, alpha = 0.05, nDigits = 2)
computeStats2(model, "pcmb 0.03", n, alpha = 0.05, nDigits = 2)
computeStats2(model, "iamb 5e-04", n, alpha = 0.05, nDigits = 2)

#################################################################################################
# testing using optimized prior/critical value
allNodes = bnlearn::nodes(dag)
datasets = list.files(paste0(model, "/data testing rds/"), pattern = paste0("_", n, "_"))
for (ii in 1:length(datasets)) {
  
  data = readRDS(paste0(model, "/data testing rds/", datasets[ii]))

  # prepare for mmlCPT
  dataInfo = getDataInfo(data) 
  mbList = list()
  
  # compute mb of each node using standard forward selection
  for (i in 1:length(allNodes)) {
    
    targetNode = allNodes[i]
    mbList[[i]] = mbForwardSelection.fast(data, targetNode, mmlCPT.fast, dataInfo$arities, dataInfo$indexListPerNodePerValue, 
                                          base = exp(1), debug = F)
    #mbList[[i]] = mbForwardSelectionUsingMMLMultinomialDirichlet(data, targetNode, dataInfo$arities, dataInfo$indexListPerNodePerValue,
    #                                                             conPar = 1, base = exp(1), debug = T)
    
  } # end for i 
  
  saveRDS(mbList, paste0(model, "/mb/cpt std 1 testing/", datasets[ii])) # save mbList into folder
  
  # use symmetry condition to re-check for mb candidate for each node
  mbList = symmetryCheck(allNodes, mbList)
  
  saveRDS(mbList, paste0(model, "/mb/cpt sym 1 testing/", datasets[ii])) # save mbList into folder
  
} # end for ii


datasets = list.files(paste0(model, "/data testing/"), pattern = paste0("_", n, "_"))
setwd("pcmb/") # set wd to pcmb folder
file.remove("output.txt")
##### pcmb from c++
for (ii in 1:length(datasets)) {
  
  file.copy(paste0("../", model, "/data testing/", datasets[ii]), paste0(model, ".data"), overwrite = TRUE) # copy data from "alarm data" to "pcmb" with new name "alarm.data"
  
  results = system(paste0("kmb4 ", model, ".data ", n, " ", length(allNodes), " -1 1.0 1 1 0.03"), intern = TRUE)
  
  output = read.table("output.txt")[, 1] # load output file from c++
  
  mbList = parsePCMB(output, length(allNodes), allNodes)
  
  saveRDS(mbList, paste0("../", model, "/mb/pcmb 0.03 testing/", datasets[ii], ".rds")) # save learned mb as .rds
  
  file.remove("output.txt")
  
}

##### iamb from c++
for (ii in 1:length(datasets)) {
  
  file.copy(paste0("../", model, "/data testing/", datasets[ii]), paste0(model, ".data"), overwrite = TRUE) # copy data from "alarm data" to "pcmb" with new name "alarm.data"
  
  results = system(paste0("kmb4 ", model, ".data ", n, " ", length(allNodes), " -1 1.0 1 0 0.001"), intern = TRUE)
  
  output = read.table("output.txt")[, 1] # load output file from c++
  
  mbList = parsePCMB(output, length(allNodes), allNodes)
  
  saveRDS(mbList, paste0("../", model, "/mb/iamb 0.001 testing/", datasets[ii], ".rds")) # save learned mb as .rds
  
  file.remove("output.txt")
  
}

# apply symmetry check 
setwd("../")
results = list.files(paste0(model, "/mb/pcmb 0.03 testing/"), pattern = paste0("_", n, "_"))
for (i in 1:length(results)) {
  
  mbList = readRDS(paste0(model, "/mb/pcmb 0.03 testing/", results[i]))
  mbList = symmetryCheck(allNodes, mbList)
  
  saveRDS(mbList, paste0(model, "/mb/pcmb 0.03 sym testing/", results[i]))
  
}

# apply symmetry check for iamb
results = list.files(paste0(model, "/mb/iamb 0.001 testing/"), pattern = paste0("_", n, "_"))
for (i in 1:length(results)) {
  
  mbList = readRDS(paste0(model, "/mb/iamb 0.001 testing/", results[i]))
  mbList = symmetryCheck(allNodes, mbList)
  
  saveRDS(mbList, paste0(model, "/mb/iamb 0.001 sym testing/", results[i]))
  
}




computeStats2(model, "iamb 0.001 testing", n, alpha = 0.05, nDigits = 2)
computeStats2(model, "iamb 0.001 sym testing", n, alpha = 0.05, nDigits = 2)
computeStats2(model, "pcmb 0.03 testing", n, alpha = 0.05, nDigits = 2)
computeStats2(model, "pcmb 0.03 sym testing", n, alpha = 0.05, nDigits = 2)
computeStats2(model, "cpt std 1 testing", n, alpha = 0.05, nDigits = 2)
computeStats2(model, "cpt sym 1 testing", n, alpha = 0.05, nDigits = 2)













