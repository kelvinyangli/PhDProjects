# max number of samples in mrmr is the maximum number of rows exist in dataset
# since mrmr requires the number of relevant features to be pre-determined 
# we will use the number of candidates in the true markov blanket as nFeatures for mrmr 
# and we won't apply mrmr if the true markov blanket is empty
# the c++ version of mrmr takes the 1st column as the target variable 
# the returned variables are indexed from 1 to nVar - 1, which is the same as indexing all variables from 0 to nVar - 1

#setwd("realWorldModelWithTrueParameters/") # for using known models with real paremters 
model = "alarm"
cpts = read.dsc(paste0(model, "/cpts/", model, ".dsc"))
allNodes = names(cpts)
n = 1000
datasets = list.files(paste0(model, "/data rds/"), pattern = paste0("_", n, "_"))

# determine the number of features in the true mb
nFeaturesList = rep(0, length(allNodes))
for (i in 1:length(allNodes)) nFeaturesList[i] = length(bnlearn::mb(cpts, allNodes[i]))

# compute mb for each dataset
setwd("mrmr")
for (i in 1:length(datasets)) {# for each data 
  
  mbList = list()
  #data = read.table(paste0("../", model, "/data/", datasets[i]))
  data = readRDS(paste0("../", model, "/data rds/", datasets[i]))
  data=data[1:10,]
  #colnames(data) = allNodes # assign column names
  originalData = data # save data to elsewhere for furture re-order column use
  
  # for the 1st column, don't need to change its position, so just apply mrmr
  write.csv(data, "data.csv", row.names = FALSE)
  
  nFeatures = nFeaturesList[1] 
  maxSamples = n # maximum number of samples equals the n, which is the number of rows of data
  output = system(paste0("mrmr_win32 -i data.csv -n ", nFeatures, " -s ", maxSamples), intern = TRUE)
  mbList[[1]] = parseMRMR(output, nFeatures)
  
  for (j in 2:ncol(originalData)) {# for each column from the 2nd column onwards
    
    # place the jth column into the 1st column 
    indices = reOrderColumns(ncol(originalData), j)
    data = originalData[, indices]
    #print(indices)
    write.csv(data, paste0("data", j, ".csv"), row.names = FALSE)
    
    nFeatures = nFeaturesList[j]
    output = system(paste0("mrmr_win32 -i data.csv -n ", nFeatures, " -s ", maxSamples, " -v ", length(allNodes)), intern = TRUE)
    mbList[[j]] = parseMRMR(output, nFeatures)
    
  } # end for j
  
  saveRDS(mbList, paste0("../", model, "/mb/mrmr/", datasets[i], ".rds"))
  
}

setwd("../")
computeStats4(model, "mrmr", n)












