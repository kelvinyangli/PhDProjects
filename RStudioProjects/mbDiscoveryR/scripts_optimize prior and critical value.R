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
# setwd("C:/PhDProjects/RStudioProjects/mbDiscoveryR/")
setwd("C:/mbDiscoveryExperimentalResults/")
model = "34_4_4_1"
#dag = readRDS(paste0("Known BNs/", model, "Dag.rds")) # read dag, for models with uniform parameters
#arities = readRDS(paste0("Known BNs/", model, "Arity.rds")) # for models with uniform parameters
#cpts = read.dsc(paste0(model, "/cpts/", model, ".dsc")) # for models with real parameters
n = 100
nIter = 5
datasets = list.files(paste0(model, "/data training/"), pattern = paste0("_", n, "_"))
#allNodes = bnlearn::nodes(dag)
#allNodes = names(cpts)

# optimize mml prior
conPars = c(1, 10, 100)
n = 10000
datasets = list.files(paste0(model, "/data rds/"), pattern = paste0("_", n, "_"))
for (j in 1:length(conPars)) {
  
  dir.create(paste0(model, "/mb/", "cpt std dir", conPars[j]))

  for (ii in 1:length(datasets)) {
    
    data = readRDS(paste0(model, "/data rds/", datasets[ii]))
    allNodes = colnames(data)
    
    # prepare for mmlCPT
    dataInfo = getDataInfo(data) 
    mbList = list()
    
    # compute mb of each node using standard forward selection
    for (i in 1:length(allNodes)) {
      
      targetNode = allNodes[i]
      mbList[[i]] = mbForwardSelectionUsingMMLMultinomialDirichlet(data, targetNode, dataInfo$arities, dataInfo$indexListPerNodePerValue,
                                                                   conPar = conPars[j], base = exp(1), debug = F)
      
    } # end for i 
    
    saveRDS(mbList, paste0(model, "/mb/cpt std dir", conPars[j], "/", datasets[ii])) # save mbList into folder
  
  }
  
}

for (j in 1:length(conPars)) print(computeStats(model, paste0("cpt std dir", conPars[j]), n, nIter = 5, others = NULL))



# optimize critical value for pcmb
alpha = c(0.01)
#res = c()
models = list.files(paste0(model, "/cpts asymDir testing/"))
dags = list.files(paste0(model, "/dag asymDir testing/"))
datasets = list.files(paste0(model, "/data asymDir testing/"), pattern = paste0("_", n, "_"))
setwd("pcmb/")
for (i in 1:length(alpha)) {
  
  dir.create(paste0("../", model, "/mb/aymDir pcmb ", alpha[i]))
  #setwd("pcmb/")
  file.remove("output.txt")
  
  ##### pcmb from c++
  for (ii in 1:length(datasets)) {
    
    file.copy(paste0("../", model, "/data asymDir testing/", datasets[ii]), "synModel.data", overwrite = TRUE) 
    fileName = strsplit(models[ceiling(ii / nIter)], ".rds")[[1]]
    fileName = paste0(fileName, ".data.net")
    file.copy(paste0("../", model, "/dag asymDir testing/", fileName), "synModel.data.net", overwrite = TRUE)
    
    cpts = readRDS(paste0("../", model, "/cpts asymDir testing/", models[ceiling(ii / nIter)]))
    
    results = system(paste0("kmb4 synModel.data ", n, " ", length(cpts), " -1 1.0 1 1 ", alpha[i]), intern = TRUE)
    
    output = read.table("output.txt")[, 1] # load output file from c++
    
    mbList = parsePCMB(output, length(cpts), names(cpts))
    
    saveRDS(mbList, paste0("../", model, "/mb/asymDir pcmb ", alpha[i], "/", datasets[ii], ".rds")) # save learned mb as .rds
    
    file.remove("output.txt")
    
  }
  
}


# optimize critical value for iamb
alpha = 0.01
#res = c()
setwd("pcmb/")
for (i in 1:length(alpha)) {
  
  dir.create(paste0("../", model, "/mb/asymDir iamb ", alpha[i]))
  file.remove("output.txt")
  
  ##### pcmb from c++
  for (ii in 1:length(datasets)) {
    
    file.copy(paste0("../", model, "/data asymDir testing/", datasets[ii]), "synModel.data", overwrite = TRUE) 
    fileName = strsplit(models[ceiling(ii / nIter)], ".rds")[[1]]
    fileName = paste0(fileName, ".data.net")
    file.copy(paste0("../", model, "/dag asymDir testing/", fileName), "synModel.data.net", overwrite = TRUE)
    
    cpts = readRDS(paste0("../", model, "/cpts asymDir testing/", models[ceiling(ii / nIter)]))
    
    results = system(paste0("kmb4 synModel.data ", n, " ", length(cpts), " -1 1.0 1 0 ", alpha[i]), intern = TRUE)
    
    output = read.table("output.txt")[, 1] # load output file from c++
    
    mbList = parsePCMB(output, length(cpts), names(cpts))
    
    saveRDS(mbList, paste0("../", model, "/mb/asymDir iamb ", alpha[i], "/", datasets[ii], ".rds")) # save learned mb as .rds
    
    file.remove("output.txt")
    
  }
  
}


setwd("../")
# check accuracy for model with uniform parameters
#computeStats2(model, "cpt std 0.3", n, alpha = 0.05, nDigits = 2)
#computeStats2(model, "pcmb 0.03", n, alpha = 0.05, nDigits = 2)
#computeStats2(model, "iamb 5e-04", n, alpha = 0.05, nDigits = 2)

# check accuracy for models with real parameters
#computeStats4(model, "cpt std 0.1", n, alpha = 0.05, nDigits = 2)

computeStats(model, "asymDir iamb 0.01", n, nIter = 5, others = "asymDir testing")

for (i in 1:length(alpha)) print(computeStats(model, paste0("iamb ", alpha[i]), n, nIter = 5, others = "training"))

#################################################################################################
# testing using optimized prior/critical value
#allNodes = bnlearn::nodes(dag)
#model = "alarm"
n = 100
datasets = list.files(paste0(model, "/data rds/"), pattern = paste0("_", n, "_"))
for (ii in 1:length(datasets)) {
  
  data = readRDS(paste0(model, "/data rds/", datasets[ii]))
  allNodes = names(data)
  
  # prepare for mmlCPT
  dataInfo = getDataInfo(data) 
  mbList = list()
  
  # compute mb of each node using standard forward selection
  for (i in 1:length(allNodes)) {
    
    targetNode = allNodes[i]
    mbList[[i]] = mbForwardSelection.fast(data, targetNode, mmlCPT.fast, dataInfo$arities, dataInfo$indexListPerNodePerValue, 
                                          base = exp(1), debug = F)
    #mbList[[i]] = mbForwardSelectionUsingMMLMultinomialDirichlet(data, targetNode, dataInfo$arities, dataInfo$indexListPerNodePerValue,
    #                                                             conPar = 0.7, base = exp(1), debug = F)
    
  } # end for i 
  
  saveRDS(mbList, paste0(model, "/mb/cpt std 1 testing/", datasets[ii])) # save mbList into folder
  
  # use symmetry condition to re-check for mb candidate for each node
  mbList = symmetryCheck(allNodes, mbList)
  
  saveRDS(mbList, paste0(model, "/mb/cpt sym 1 testing/", datasets[ii])) # save mbList into folder
  
} # end for ii


dags = list.files(paste0(model, "/dag testing/"))
models = list.files(paste0(model, "/cpts testing/"))
datasets = list.files(paste0(model, "/data testing/"), pattern = paste0("_", n, "_"))
setwd("pcmb2/") # set wd to pcmb folder
file.remove("output.txt")
##### pcmb from c++
for (ii in 1:length(datasets)) {
  
  file.copy(paste0("../", model, "/data testing/", datasets[ii]), "synModel.data", overwrite = TRUE) 
  file.copy(paste0("../", model, "/dag testing/", dags[ceiling(ii / nIter)]), "synModel.data.net", overwrite = TRUE)
  
  cpts = readRDS(paste0("../", model, "/cpts testing/", models[ceiling(ii / nIter)]))
  
  results = system(paste0("kmb4 synModel.data ", n, " ", length(cpts), " -1 1.0 1 1 0.1"), intern = TRUE)
  
  output = read.table("output.txt")[, 1] # load output file from c++
  
  mbList = parsePCMB(output, length(cpts), names(cpts))
  
  saveRDS(mbList, paste0("../", model, "/mb/pcmb 0.1 testing/", datasets[ii], ".rds")) # save learned mb as .rds
  
  file.remove("output.txt")
  
}

##### iamb from c++
file.remove("output.txt")
for (ii in 1:length(datasets)) {
  
  file.copy(paste0("../", model, "/data testing/", datasets[ii]), "synModel.data", overwrite = TRUE) 
  file.copy(paste0("../", model, "/dag testing/", dags[ceiling(ii / nIter)]), "synModel.data.net", overwrite = TRUE)
  
  cpts = readRDS(paste0("../", model, "/cpts testing/", models[ceiling(ii / nIter)]))
  
  results = system(paste0("kmb4 synModel.data ", n, " ", length(cpts), " -1 1.0 1 0 0.001"), intern = TRUE)
  
  output = read.table("output.txt")[, 1] # load output file from c++
  
  mbList = parsePCMB(output, length(cpts), names(cpts))
  
  saveRDS(mbList, paste0("../", model, "/mb/iamb 0.001 testing/", datasets[ii], ".rds")) # save learned mb as .rds
  
  file.remove("output.txt")
  
}

# apply symmetry check 
setwd("../")
results = list.files(paste0(model, "/mb/pcmb 0.1 testing/"), pattern = paste0("_", n, "_"))
models = list.files(paste0(model, "/cpts testing/"))
for (i in 1:length(results)) {
  
  mbList = readRDS(paste0(model, "/mb/pcmb 0.1 testing/", results[i]))
  cpts = readRDS(paste0(model, "/cpts testing/", models[ceiling(i / nIter)]))
  allNodes = names(cpts)
  mbList = symmetryCheck(allNodes, mbList)
  
  saveRDS(mbList, paste0(model, "/mb/pcmb 0.1 sym testing/", results[i]))
  
}

# apply symmetry check for iamb
results = list.files(paste0(model, "/mb/iamb 0.001 testing/"), pattern = paste0("_", n, "_"))
for (i in 1:length(results)) {
  
  mbList = readRDS(paste0(model, "/mb/iamb 0.001 testing/", results[i]))
  cpts = readRDS(paste0(model, "/cpts testing/", models[ceiling(i / nIter)]))
  allNodes = names(cpts)
  mbList = symmetryCheck(allNodes, mbList)
  
  saveRDS(mbList, paste0(model, "/mb/iamb 0.001 sym testing/", results[i]))
  
}




#computeStats4(model, "iamb 0.007 testing", n, alpha = 0.05, nDigits = 2)
#computeStats4(model, "iamb 0.007 sym testing", n, alpha = 0.05, nDigits = 2)
#computeStats4(model, "pcmb 0.11 testing", n, alpha = 0.05, nDigits = 2)
#computeStats4(model, "pcmb 0.11 sym testing", n, alpha = 0.05, nDigits = 2)
#computeStats4(model, "cpt std 0.1 testing", n, alpha = 0.05, nDigits = 2)
#computeStats4(model, "cpt sym 0.1 testing", n, alpha = 0.05, nDigits = 2)


computeStats(model, "iamb 0.001 testing", n, nIter = 5, others = "testing")
computeStats(model, "iamb 0.001 sym testing", n, nIter = 5, others = "testing")
computeStats(model, "pcmb 0.1 testing", n, nIter = 5, others = "testing")
computeStats(model, "pcmb 0.1 sym testing", n, nIter = 5, others = "testing")
computeStats(model, "cpt std 0.7 testing", n, nIter = 5, others = "testing")
computeStats(model, "cpt sym 0.7 testing", n, nIter = 5, others = "testing")


computeStats(model, "cpt std 1 testing", n, nIter = 5, others = "testing")
computeStats(model, "cpt sym 1 testing", n, nIter = 5, others = "testing")






