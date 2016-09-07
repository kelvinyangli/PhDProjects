# get the learned mb for each model and compute the avearge with confidence interval 

computeStats = function(model, method, n, nIter = 10, alpha = 0.05, nDigits = 2) {
  
  models = list.files(paste0(model, "/cpts/"))
  files = list.files(paste0(model, "/mb/", method), pattern = paste0(model, "_", n, "_"))
  
  mtx = matrix(0, nrow = length(files), ncol = 4)
  
  for (i in 1:length(models)) { # for each model
    
    cpts = readRDS(paste0(model, "/cpts/", models[i])) # load true cpts
    allNodes = names(cpts)
    
    for (j in ((i - 1) * nIter + 1):(i * nIter)) { # for each dataset
      
      # columns = "precision", "recall", "distance", "fmeasure")
      resultsMatrix = matrix(0, nrow = length(cpts), ncol = 4)
      
      mbList = readRDS(paste0(model, "/mb/", method, "/", files[j])) # load learned mb
      
      for (k in 1:length(cpts)) {
        
        mbTrue = bnlearn::mb(cpts, allNodes[k])
        
        mbLearned = mbList[[k]]
          
        res = mbAccuracy(mbTrue, mbLearned, allNodes[k], allNodes)
        
        precision = res$precision
        recall = res$recall
        
        if ((precision + recall) == 0) { # if both precision and recall = 0 then f measure = 0
          
          distance = sqrt(2)
          fmeasure = 0
          
        } else { # else compute the harmonic mean of precision and recall
          
          distance = sqrt((1 - precision) ^ 2 + (1 - recall) ^ 2)
          fmeasure = 2 * precision * recall / (precision + recall)
          
        } # end else 
        
        resultsMatrix[k, ] = c(precision, recall, distance, fmeasure)
        
      } # end for k
      
      mtx[j, ] = colMeans(resultsMatrix)
      
    } # end for j
    
  } # end for i
  
  write.csv(mtx, paste0("results_synthetic/", model, "_", method, "_", n, ".csv"), row.names = FALSE)
  
  return(round(computeCI(mtx, alpha = alpha), nDigits))
    
}


computeStats2 = function(model, method, n, nIter = 10, alpha = 0.05, nDigits = 2) {
  
  dag = readRDS(paste0("Known BNs/", model, "Dag.rds"))
  allNodes = bnlearn::nodes(dag)
  files = list.files(paste0(model, " mb/", method), pattern = paste0(model, "_", n, "_"))
  
  mtx = matrix(0, nrow = length(files), ncol = 4)
  
  for (i in 1:length(files)) { # for each files
    
    # columns = "precision", "recall", "distance", "fmeasure")
    resultsMatrix = matrix(0, nrow = length(allNodes), ncol = 4)
    
    mbList = readRDS(paste0(model, " mb/", method, "/", files[i])) # load learned mb
    
    for (j in 1:length(allNodes)) {
      
      mbTrue = bnlearn::mb(dag, allNodes[j])
      
      mbLearned = mbList[[j]]
      
      res = mbAccuracy(mbTrue, mbLearned, allNodes[j], allNodes)
      
      precision = res$precision
      recall = res$recall
      
      if ((precision + recall) == 0) { # if both precision and recall = 0 then f measure = 0
        
        distance = sqrt(2)
        fmeasure = 0
        
      } else { # else compute the harmonic mean of precision and recall
        
        distance = sqrt((1 - precision) ^ 2 + (1 - recall) ^ 2)
        fmeasure = 2 * precision * recall / (precision + recall)
        
      } # end else 
      
      resultsMatrix[j, ] = c(precision, recall, distance, fmeasure)
      
    } # end for j
    
    mtx[i, ] = colMeans(resultsMatrix)
    
  } # end for i
  
  write.csv(mtx, paste0("results_known/", model, "_", method, "_", n, ".csv"), row.names = FALSE)
  
  return(round(computeCI(mtx, alpha = alpha), nDigits))
  
}


