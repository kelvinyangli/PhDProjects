# compute mean and sd for known BNs
autoStatsKnownBN = function(currentDirectory, learningMethod, measure, vector, numIterations, alpha = 0.05) {
  
  average = rep(0, length(vector) / numIterations)
  
  std = average
  
  for (i in 1:length(average)) {
    
    average[i] = mean(vector[((i - 1) * numIterations + 1):(i * numIterations)])
    
    std[i] = sd(vector[((i - 1) * numIterations + 1):(i * numIterations)])
    
  }
  
  error = qnorm(1 - alpha / 2) * std / sqrt(numIterations)
  
  stats = data.frame(average, std, error)
  
  write.csv(stats, paste0(currentDirectory, "/Evaluations/", measure, "/stats/stats_", learningMethod, ".csv"), row.names = FALSE)
  
}