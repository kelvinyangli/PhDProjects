# scripts on evaluating known models with build in parameters
setwd("realWorldModelWithTrueParameters")
model = "hailfinder"
n = 10000
#beta = 1
nIter = 20


cpts = read.dsc(paste0(model, "/cpts/", model, ".dsc"))
allNodes = names(cpts)

# generate data 
for (j in 1:nIter) { 
  
  # sample data
  seed = generateSeed()
  set.seed(seed)
  data = rbn(cpts, n)
  
  fileName = paste(model, n, seed, sep = "_")
  saveRDS(data, paste0(model, "/data rds/", fileName, ".rds"))
  write.table(data, paste0(model, "/data/", fileName, ".data"), row.names = FALSE, col.names = FALSE) # save data
  
  #Sys.sleep(0.01) # suspend execution for 0.01 seconds to avoid generating the same seed
  
} # end for j
  

# apply mmlCPT 
datasets = list.files(paste0(model, "/data rds/"), pattern = paste0("_", n, "_"))
#datasets = list.files(paste0(model, "/data/"), pattern = paste0("_", n, "_"))
for (ii in 1:length(datasets)) {
  
  #data = read.table(paste0(model, "/data/", datasets[ii]))
  # may need/need not this steps to convert variables into factors
  #temp = read.table(paste0(model, "/data/", datasets[ii]))
  data = readRDS(paste0(model, "/data rds/", datasets[ii]))
  #data = temp
  #for (k in 1:ncol(temp)) data[,k] = as.factor(temp[,k])
  
  #colnames(data) = allNodes
  
  # prepare for mmlCPT
  dataInfo = getDataInfo(data) 
  mbList = list()
  
  # compute mb of each node using standard forward selection
  for (i in 1:length(allNodes)) {
    
    targetNode = allNodes[i]
    mbList[[i]] = mbForwardSelection.fast(data, targetNode, dataInfo$arities, dataInfo$indexListPerNodePerValue)
    
  } # end for i 
  
  #fileName = strsplit(datasets[ii], ".data")[[1]]
  #saveRDS(mbList, paste0(model, "/mb/cpt std/", fileName, ".rds")) # save mbList into folder
  saveRDS(mbList, paste0(model, "/mb/cpt std/", datasets[ii]))
  
  # use symmetry condition to re-check for mb candidate for each node
  mbList = symmetryCheck(allNodes, mbList)
  
  #saveRDS(mbList, paste0(model, "/mb/cpt sym/", fileName, ".rds")) # save mbList into folder
  saveRDS(mbList, paste0(model, "/mb/cpt sym/", datasets[ii]))
  
} # end for ii


##############################################################################################################
##### pcmb from c++
datasets = list.files(paste0(model, "/data/"), pattern = paste0("_", n, "_"))

setwd("pcmb2/") # set wd to pcmb folder

file.remove("output.txt")

for (ii in 1:length(datasets)) {
  
  file.copy(paste0("../", model, "/data/", datasets[ii]), paste0(model, ".data"), overwrite = TRUE) # copy data from "alarm data" to "pcmb" with new name "alarm.data"
  
  results = system(paste0("kmb4 ", model, ".data ", n, " ", length(allNodes), " -1 1.0 1 1 0.01"), intern = TRUE)
  
  output = read.table("output.txt")[, 1] # load output file from c++
  
  mbList = parsePCMB(output, length(allNodes), allNodes)
  
  fileName = strsplit(datasets[ii], ".data")[[1]]
  saveRDS(mbList, paste0("../", model, "/mb/pcmb test/", fileName, ".rds")) # save learned mb as .rds
  
  file.remove("output.txt")
  
}

##### iamb from c++
file.remove("output.txt")

for (ii in 1:length(datasets)) {
  
  file.copy(paste0("../", model, "/data/", datasets[ii]), paste0(model, ".data"), overwrite = TRUE) # copy data from "alarm data" to "pcmb" with new name "alarm.data"
 
  results = system(paste0("kmb4 ", model, ".data ", n, " ", length(allNodes), " -1 1.0 1 0 0.01"), intern = TRUE)
  
  output = read.table("output.txt")[, 1] # load output file from c++
  
  mbList = parsePCMB(output, length(allNodes), allNodes)
  
  fileName = strsplit(datasets[ii], ".data")[[1]]
  saveRDS(mbList, paste0("../", model, "/mb/iamb/", fileName, ".rds")) # save learned mb as .rds
  
  file.remove("output.txt")
  
}

setwd("../")

# apply symmetry check for pcmb
results = list.files(paste0(model, "/mb/pcmb/"), pattern = paste0("_", n, "_"))
for (i in 1:length(results)) {
  
  mbList = readRDS(paste0(model, "/mb/pcmb/", results[i]))
  mbList = symmetryCheck(allNodes, mbList)
  
  saveRDS(mbList, paste0(model, "/mb/pcmb sym/", results[i]))
  
}


# apply symmetry check for iamb
results = list.files(paste0(model, "/mb/iamb/"), pattern = paste0("_", n, "_"))
for (i in 1:length(results)) {
  
  mbList = readRDS(paste0(model, "/mb/iamb/", results[i]))
  mbList = symmetryCheck(allNodes, mbList)
  
  saveRDS(mbList, paste0(model, "/mb/iamb sym/", results[i]))
  
}



computeStats4(model, "iamb", n, alpha = 0.05, nDigits = 2)
computeStats4(model, "iamb sym", n, alpha = 0.05, nDigits = 2)
computeStats4(model, "pcmb test", n, alpha = 0.05, nDigits = 2)
computeStats4(model, "pcmb sym", n, alpha = 0.05, nDigits = 2)
computeStats4(model, "cpt std", n, alpha = 0.05, nDigits = 2)
computeStats4(model, "cpt sym", n, alpha = 0.05, nDigits = 2)




