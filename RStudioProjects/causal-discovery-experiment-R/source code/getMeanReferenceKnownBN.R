getMeanReferenceKnownBN = function(currentDirectory, numIterations, alpha = 0.05) {
  
  files = list.files(paste0(currentDirectory, "/Evaluations/reference"), pattern = ".csv")
  
  files = orderFilesKnownBN(files, numIterations)
  
  refRMSEs = vector(length = length(files))
  refBIRs = vector(length = length(files))
  
  for (i in 1:length(files)) { # for each file, take the average reference over all variables
    
    ref = read.csv(paste0(currentDirectory, "/Evaluations/reference/", files[i]), header = TRUE)
    
    refRMSEs[i] = mean(ref$rmse)
    refBIRs[i] = mean(ref$bir)
    
  }
  
  write.csv(refRMSEs, paste0(currentDirectory, "/Evaluations/reference/stats/", "refRMSEs.csv"), row.names = FALSE)
  write.csv(refBIRs, paste0(currentDirectory, "/Evaluations/reference/stats/", "refBIRs.csv"), row.names = FALSE)
  
  autoStatsKnownBN(currentDirectory, "refRMSEs", "reference", refRMSEs, numIterations, alpha = alpha)
  autoStatsKnownBN(currentDirectory, "refBIRs", "reference", refBIRs, numIterations, alpha = alpha)
  
}