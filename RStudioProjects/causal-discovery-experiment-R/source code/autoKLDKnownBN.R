##########################################################################################
# compute kld 
# R cannot compute kld for network with 50 nodes due to memory issue
##########################################################################################
autoKLDKnownBN = function(currentDirectory, learningMethod, numIterations = 20, debug = FALSE) {
  
  cptsTrue = read.dsc(paste0(currentDirectory, "/", currentDirectory, ".dsc"))
  
  allLearnedCPTs = list.files(paste0(currentDirectory, "/Learned networks/CPTs/", learningMethod))
  allLearnedCPTs = orderFilesKnownBN(allLearnedCPTs, numIterations)
  
  klDivergence = rep(0, length(allLearnedCPTs))
  
  for (i in 1:length(allLearnedCPTs)) {
    #for (i in 1:140) { # compute kld b/w each true and learned cpts
    
    cptsLearned = readRDS(paste0(currentDirectory, "/Learned networks/CPTs/", learningMethod, "/", allLearnedCPTs[i]))
    
    klDivergence[i] = kld(cptsTrue, cptsLearned, debug = FALSE)[[1]] # compute kld between true and learned
    
    #write.csv(klDivergence, paste0(currentDirectory, "/Evaluations/kld/", learningMethod, "/", allTrueCPTs[i], ".csv"), row.names = FALSE)
  } # end for i
  
  write.csv(klDivergence, paste0(currentDirectory, "/Evaluations/kld/", learningMethod, ".csv"), row.names = FALSE)
  
  autoStatsKnownBN(currentDirectory, learningMethod, "kld", klDivergence, numIterations)
  
}


# compute kld for up to max 7 parents











