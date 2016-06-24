# calculate 3 versions of hamming distance
# bnlearn::shd define on cpdags
# bnlearn::hamming define on skeletons
# hammingDags define on dags
autoEditDistanceKnownBN = function(currentDirectory, learningMethod, numIterations) {
  
  cptsTrue = read.dsc(paste0(currentDirectory, "/", currentDirectory, ".dsc"))
  
  dagTrue = model2network(modelstring(cptsTrue))
  
  allCptsLearned = list.files(paste0(currentDirectory, "/Learned networks/CPTs/", learningMethod))
  
  allCptsLearned = orderFilesKnownBN(allCptsLearned, numIterations)
  
  # 3 empty score vectors
  shdCpdags = rep(0, length(allCptsLearned)) 
  
  shdSkeletons = shdCpdags
  
  shdDags = shdCpdags
  
  for (i in 1:length(allCptsLearned)) {
    
    cptsLearned = readRDS(paste0(currentDirectory, "/Learned networks/CPTs/", learningMethod, "/", allCptsLearned[i]))
    
    dagLearned = model2network(modelstring(cptsLearned))
    
    # shd
    shdCpdags[i] = bnlearn::shd(learned = dagLearned, true = dagTrue)
    
    # hamming
    shdSkeletons[i] = hamming(learned = dagLearned, true = dagTrue)
    
    # hammingDags
    shdDags[i] = hammingDags(learned = dagLearned, true = dagTrue)
    
  }
  
  write.csv(shdCpdags, paste0(currentDirectory, "/Evaluations/shd cpdags/", learningMethod, ".csv"), row.names = FALSE)
  
  write.csv(shdSkeletons, paste0(currentDirectory, "/Evaluations/shd skeletons/", learningMethod, ".csv"), row.names = FALSE)
  
  write.csv(shdDags, paste0(currentDirectory, "/Evaluations/shd dags/", learningMethod, ".csv"), row.names = FALSE)
  
  autoStatsKnownBN(currentDirectory, learningMethod, "shd cpdags", shdCpdags, numIterations)
  
  autoStatsKnownBN(currentDirectory, learningMethod, "shd skeletons", shdSkeletons, numIterations)
  
  autoStatsKnownBN(currentDirectory, learningMethod, "shd dags", shdDags, numIterations)
  
}
