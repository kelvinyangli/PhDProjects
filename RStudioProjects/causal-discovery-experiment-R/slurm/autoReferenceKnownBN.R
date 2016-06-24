##################################################################################
# compute rmse and bir by constructing a prediction model 
# increasing prediction accuracy indicates a decreasing rmse and increasing bir
##################################################################################

source("Rcode/orderFilesKnownBN.R")

source("Rcode/autoPredictionKnownBN.R")
#######################################################################################################
# auto prediction starts here 
#######################################################################################################
autoReferenceKnownBN = function(currentDirectory, numIterations, maxNumParents, maxNumValues, debug = FALSE) {
  
  # list and order all data sets
  allDataSets = list.files(paste0(currentDirectory, "/Datasets/Numeric"))
  allDataSets = orderFilesKnownBN(allDataSets, numIterations)
  
  cptsTrue = read.dsc(paste0(currentDirectory, "/", currentDirectory, ".dsc"))
  
  taskid <- Sys.getenv("SLURM_ARRAY_TASK_ID") 
  
  fileIndex = strtoi(taskid) 
  
  cat("Our TaskID is ",fileIndex," and our filename is ",allDataSets[fileIndex],"\n")
  
  # read data
  data = readRDS(paste0(currentDirectory, "/Datasets/Numeric/", allDataSets[fileIndex]))
  
  # convert data from data.frame to matrix to speed up 
  data = as.matrix(data)
  
  #test write output to a Lustre File System
  DIR <- paste0("/home/kli/p2016040001/Experiments_23062016/", currentDirectory, "/Evaluations/reference/")
  
  # compute actual rmse and bir using 10000 samples from data
  actual = prediction(cptsTrue, data, maxNumParents, maxNumValues, debug = debug)
  
  write.csv(actual, paste0(DIR, allDataSets[fileIndex], ".csv"), row.names = FALSE)

}





