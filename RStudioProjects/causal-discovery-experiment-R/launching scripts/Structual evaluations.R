# scripts for precision, recall, and edit distance

currentDirectory = "alarm"

learningMethods = c("aic", "bic", "bde", "k2", "mmhc", "k2Matlab","pc")

# load all true cpts and convert to dags
# allTrueCPTs = list.files(paste0(currentDirectory, "/True networks/CPTs"))

# allTrueCPTs = orderFiles(allTrueCPTs, currentDirectory, 20)

# allTrueCPTsList = list()

# allTrueDagsList = list()

#for (i in 1:length(allTrueCPTs)) {
  
#  allTrueCPTsList[[i]] = readRDS(paste0(currentDirectory, "/True networks/CPTs/", allTrueCPTs[i]))
  
#  allTrueDagsList[[i]] = model2network(modelstring(allTrueCPTsList[[i]]))
  
#}

# accuracies
sapply(learningMethods, autoAccuracyKnownBN, currentDirectory = currentDirectory, numIterations = 20)

# precision and recall
autoPrecisionRecallKnownBN(currentDirectory = currentDirectory, numIterations = 20)

# f measure
autoFMeasureKnownBN(currentDirectory = currentDirectory, numIterations = 20)

# edit distance
sapply(learningMethods, autoEditDistanceKnownBN, currentDirectory = currentDirectory, numIterations = 20)





