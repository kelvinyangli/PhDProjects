autoPredictionKnownBN = function(currentDirectory, numIterations, maxNumParents, maxNumValues, debug = FALSE) {
  
  cptsTrue = read.dsc(paste0(currentDirectory, "/", currentDirectory, ".dsc"))
  
  # list and order all learned cpts
  # since learned cpts using different methods are saved with the same name but under different folders, so it's ok to 
  # just list the learned cpts from one method and use them for all other methods
  learnedCPTs = list.files(paste0(currentDirectory, "/Learned networks/CPTs/mmhc"))
  learnedCPTs = orderFilesKnownBN(learnedCPTs, numIterations)
  
  # list and order all data sets
  allDataSets = list.files(paste0(currentDirectory, "/Datasets/Numeric"))
  allDataSets = orderFilesKnownBN(allDataSets, numIterations)
  
  # list all learning methods 
  learningMethods = c("aic", "bic", "bde", "k2", "mmhc", "k2Matlab", "pc")
  
  # compute the reference and actual rmse for each data set
  for (i in 1:length(allDataSets)) {
    
    # read test data
    # these data have been converted to numeric format and the sample size is 10,000
    data = readRDS(paste0(currentDirectory, "/Datasets/Numeric/", allDataSets[i]))
    
    # compute reference rmse and bir using 10,000 samples from test data (numeric format)
    reference = prediction(cptsTrue, data, maxNumParents, maxNumValues, debug = debug)
    
    # save results to a csv file
    write.csv(reference, paste0(currentDirectory, "/Evaluations/reference/", allDataSets[i], ".csv"), row.names = FALSE)
    
    # compute actual rmse and bir for each learning method
    for (j in 1:length(learningMethods)) {
      
      # read learned cpts
      cptsLearned = readRDS(paste0(currentDirectory, "/Learned networks/CPTs/", learningMethods[j], "/", learnedCPTs[i]))
      
      # compute actual rmse and bir using 10000 samples from test data (numeric format)
      actual = prediction(cptsLearned, data, maxNumParents, maxNumValues, debug = debug)
      
      write.csv(actual$rmse, paste0(currentDirectory, "/Evaluations/rmse/", learningMethods[j], "/", allDataSets[i], ".csv"), row.names = FALSE)
      
      write.csv(actual$bir, paste0(currentDirectory, "/Evaluations/bir/", learningMethods[j], "/", allDataSets[i], ".csv"), row.names = FALSE)
      
    }
    
  }
  
}

