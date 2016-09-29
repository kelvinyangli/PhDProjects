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




setwd("../")
for (i in 1:length(alpha)) print(computeStats2(model, paste0("pcmb ", alpha[i]), n, alpha = 0.05, nDigits = 2))

results = list.files(paste0(model, "/mb/iamb 7e-04/"), pattern = paste0("_", n, "_"))
for (i in 1:length(results)) {
  
  mbList = readRDS(paste0(model, "/mb/iamb 7e-04/", results[i]))
  mbList = symmetryCheck(allNodes, mbList)
  
  saveRDS(mbList, paste0(model, "/mb/iamb 7e-04 sym/", results[i]))
  
}















