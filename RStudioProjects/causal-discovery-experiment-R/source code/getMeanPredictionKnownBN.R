getMeanPredictionKnownBN = function(currentDirectory, learningMethod, measure, numIterations, alpha = 0.05) {
  
  files = list.files(paste0(currentDirectory, "/Evaluations/", measure, "/", learningMethod))
  
  files = orderFilesKnownBN(files, numIterations)
  
  scores = vector(length = length(files))
  
  for (i in 1:length(files)) { # for each file, take the average rmse over all variables
    
    score = read.csv(paste0(currentDirectory, "/Evaluations/", measure, "/", learningMethod, "/", files[i]), header = TRUE)
    
    scores[i] = colMeans(score)
    
  }
  
  write.csv(scores, paste0(currentDirectory, "/Evaluations/", measure, "/", learningMethod, ".csv"), row.names = FALSE)
  
  autoStatsKnownBN(currentDirectory, learningMethod, measure, scores, numIterations, alpha = alpha)
  
}
