# scripts for training optimal prior for mml and critical value for ci-based methods, i.e pcmb and iamb
#libraries = c("bnlearn", "pcalg", "gRain", "gtools", "entropy", "reshape2", "ggplot2", "Rgraphviz")
#lapply(libraries, require, character.only = TRUE)

library(bnlearn)
library(gtools)
options(scipen = 10)
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
#dag = readRDS(paste0("Known BNs/", model, "Dag.rds")) # read dag, for models with uniform parameters
#arities = readRDS(paste0("Known BNs/", model, "Arity.rds")) # for models with uniform parameters
cpts = read.dsc(paste0(model, "/cpts/", model, ".dsc")) # for models with real parameters
n = 500
datasets = list.files(paste0(model, "/data training/"), pattern = paste0("_", n, "_"))
#allNodes = bnlearn::nodes(dag)
allNodes = names(cpts)

# optimize mml prior
conPars = seq(0.01, 0.1, 0.02)
datasets = list.files(paste0(model, "/data training rds/"), pattern = paste0("_", n, "_"))
for (j in 1:length(conPars)) {
  
  dir.create(paste0("alarm/mb/", "cpt std ", conPars[j]))

  for (ii in 1:length(datasets)) {
    
    data = readRDS(paste0(model, "/data training rds/", datasets[ii]))
    #colnames(data) = allNodes
    
    # prepare for mmlCPT
    dataInfo = getDataInfo(data) 
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
alpha = c(0.005,0.007,0.009)
#res = c()
setwd("pcmb/")
for (i in 1:length(alpha)) {
  
  dir.create(paste0("../alarm/mb/", "pcmb ", alpha[i]))
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
alpha = seq(0.001, 0.01, 0.002)
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


setwd("../")
# check accuracy for model with uniform parameters
#computeStats2(model, "cpt std 0.3", n, alpha = 0.05, nDigits = 2)
#computeStats2(model, "pcmb 0.03", n, alpha = 0.05, nDigits = 2)
#computeStats2(model, "iamb 5e-04", n, alpha = 0.05, nDigits = 2)

# check accuracy for models with real parameters
computeStats4(model, "cpt std 0.09", n, alpha = 0.05, nDigits = 2)


#################################################################################################
# testing using optimized prior/critical value
#allNodes = bnlearn::nodes(dag)
model = "alarm"
n = 10000
datasets = list.files(paste0(model, "/data testing rds/"), pattern = paste0("_", n, "_"))
for (ii in 1:length(datasets)) {
  
  data = readRDS(paste0(model, "/data testing rds/", datasets[ii]))

  # prepare for mmlCPT
  dataInfo = getDataInfo(data) 
  mbList = list()
  
  # compute mb of each node using standard forward selection
  for (i in 1:length(allNodes)) {
    
    targetNode = allNodes[i]
    #mbList[[i]] = mbForwardSelection.fast(data, targetNode, mmlCPT.fast, dataInfo$arities, dataInfo$indexListPerNodePerValue, 
    #                                      base = exp(1), debug = F)
    mbList[[i]] = mbForwardSelectionUsingMMLMultinomialDirichlet(data, targetNode, dataInfo$arities, dataInfo$indexListPerNodePerValue,
                                                                 conPar = 0.1, base = exp(1), debug = F)
    
  } # end for i 
  
  saveRDS(mbList, paste0(model, "/mb/cpt std 0.1 testing/", datasets[ii])) # save mbList into folder
  
  # use symmetry condition to re-check for mb candidate for each node
  mbList = symmetryCheck(allNodes, mbList)
  
  saveRDS(mbList, paste0(model, "/mb/cpt sym 0.1 testing/", datasets[ii])) # save mbList into folder
  
} # end for ii


datasets = list.files(paste0(model, "/data testing/"), pattern = paste0("_", n, "_"))
setwd("pcmb3/") # set wd to pcmb folder
file.remove("output.txt")
##### pcmb from c++
for (ii in 1:length(datasets)) {
  
  file.copy(paste0("../", model, "/data testing/", datasets[ii]), paste0(model, ".data"), overwrite = TRUE) # copy data from "alarm data" to "pcmb" with new name "alarm.data"
  
  results = system(paste0("kmb4 ", model, ".data ", n, " ", length(allNodes), " -1 1.0 1 1 0.11"), intern = TRUE)
  
  output = read.table("output.txt")[, 1] # load output file from c++
  
  mbList = parsePCMB(output, length(allNodes), allNodes)
  
  saveRDS(mbList, paste0("../", model, "/mb/pcmb 0.11 testing/", datasets[ii], ".rds")) # save learned mb as .rds
  
  file.remove("output.txt")
  
}

##### iamb from c++
for (ii in 1:length(datasets)) {
  
  file.copy(paste0("../", model, "/data testing/", datasets[ii]), paste0(model, ".data"), overwrite = TRUE) # copy data from "alarm data" to "pcmb" with new name "alarm.data"
  
  results = system(paste0("kmb4 ", model, ".data ", n, " ", length(allNodes), " -1 1.0 1 0 0.007"), intern = TRUE)
  
  output = read.table("output.txt")[, 1] # load output file from c++
  
  mbList = parsePCMB(output, length(allNodes), allNodes)
  
  saveRDS(mbList, paste0("../", model, "/mb/iamb 0.007 testing/", datasets[ii], ".rds")) # save learned mb as .rds
  
  file.remove("output.txt")
  
}

# apply symmetry check 
setwd("../")
results = list.files(paste0(model, "/mb/pcmb 0.11 testing/"), pattern = paste0("_", n, "_"))
for (i in 1:length(results)) {
  
  mbList = readRDS(paste0(model, "/mb/pcmb 0.11 testing/", results[i]))
  mbList = symmetryCheck(allNodes, mbList)
  
  saveRDS(mbList, paste0(model, "/mb/pcmb 0.11 sym testing/", results[i]))
  
}

# apply symmetry check for iamb
results = list.files(paste0(model, "/mb/iamb 0.007 testing/"), pattern = paste0("_", n, "_"))
for (i in 1:length(results)) {
  
  mbList = readRDS(paste0(model, "/mb/iamb 0.007 testing/", results[i]))
  mbList = symmetryCheck(allNodes, mbList)
  
  saveRDS(mbList, paste0(model, "/mb/iamb 0.007 sym testing/", results[i]))
  
}




computeStats4(model, "iamb 0.007 testing", n, alpha = 0.05, nDigits = 2)
computeStats4(model, "iamb 0.007 sym testing", n, alpha = 0.05, nDigits = 2)
computeStats4(model, "pcmb 0.11 testing", n, alpha = 0.05, nDigits = 2)
computeStats4(model, "pcmb 0.11 sym testing", n, alpha = 0.05, nDigits = 2)
computeStats4(model, "cpt std 0.1 testing", n, alpha = 0.05, nDigits = 2)
computeStats4(model, "cpt sym 0.1 testing", n, alpha = 0.05, nDigits = 2)













