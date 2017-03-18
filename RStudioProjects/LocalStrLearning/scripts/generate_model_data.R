# generating random polytree adjmtx and cpts at the given specification
# sampling data from polytree models for given sample sizes 
nVars = 20
maxNPas = 3
maxArity = 4
beta = 1
n = 100
nRepeat = 5 # the number of times repeat this experiment
dir = "../../../Dag experiments/"

for (i in 1:1) { 
  
  for (nModel in 1:nRepeat) {
    
    model_seed = randSeed()
    set.seed(model_seed)
    adjmtx = randAdjmtx(nVars[i], maxNPas[i])
    dag = matrix2dag(adjmtx) # adjmtx -> bnlearn format
    cpts = randCPTs(dag, maxArity[i], beta[i])
    
    spec = paste(nVars[i], maxNPas[i], maxArity[i], beta[i], sep = "_")
    model_name = paste(spec, model_seed, sep = "_")
    ls = list("adjmtx" = adjmtx, "cpts" = cpts) # combine adjmtx and cpts into a list
    saveRDS(ls, paste0(dir, "/dag/", model_name, ".rds")) # save list 
    
    for (nData in 1:nRepeat) {
      
      data_seed = randSeed()
      set.seed(data_seed)
      data_1k = rbn(cpts, n[1]) # sample 1k samples
      #data_10k = rbn(cpts, n[2]) # sample 10k samples
      
      data_1k_name = paste(model_name, n[1], data_seed, sep = "_")
      #data_10k_name = paste(model_name, n[2], data_seed, sep = "_")
      saveRDS(data_1k, paste0(dir, "data/", data_1k_name, ".rds"))
      write.csv(data_1k, paste0(dir, "data_csv/", data_1k_name, ".csv"), row.names = F)
      #saveRDS(data_10k, paste0(dir, "data/", data_10k_name, ".rds"))
      #write.csv(data_10k, paste0(dir, "data_csv/", data_10k_name, ".csv"), row.names = F)
      
    } # end for nData
    
  } # end for nModel
  
} # end for i
  


  