# this script randomly generate pt adjmtx and cpts then sample data from these 
# models 
nVars = c(20, 40, 60, 80)
maxNPas = 2:5
maxArity = 3:6
beta = c(1, 5, 10, 15)
n = c(1000, 10000)
nRepeat = 10 # the number of times repeat this experiment
dir = "../../../Polytree experiments/"

for (i in 1:4) { 
  
  for (nModel in 1:nRepeat) {
    
    model_seed = randSeed()
    set.seed(model_seed)
    adjmtx = randPolytree(nVars[i], maxNPas[i])
    pt = matrix2dag(adjmtx) # adjmtx -> bnlearn format
    cpts = randCPTs(pt, maxArity[i], beta[i])
    
    spec = paste(nVars[i], maxNPas[i], maxArity[i], beta[i], sep = "_")
    model_name = paste(spec, model_seed, sep = "_")
    ls = list("adjmtx" = adjmtx, "cpts" = cpts) # combine adjmtx and cpts into a list
    saveRDS(ls, paste0(dir, "/pt/", model_name, ".rds")) # save list 
    
    for (nData in 1:nRepeat) {
      
      data_seed = randSeed()
      set.seed(data_seed)
      data_1k = rbn(cpts, n[1]) # sample 1k samples
      data_10k = rbn(cpts, n[2]) # sample 10k samples
      
      data_1k_name = paste(model_name, n[1], data_seed, sep = "_")
      data_10k_name = paste(model_name, n[2], data_seed, sep = "_")
      saveRDS(data_1k, paste0(dir, "data/", data_1k_name, ".rds"))
      write.csv(data_1k, paste0(dir, "data_csv/", data_1k_name, ".csv"), row.names = F)
      saveRDS(data_10k, paste0(dir, "data/", data_10k_name, ".rds"))
      write.csv(data_10k, paste0(dir, "data_csv/", data_10k_name, ".csv"), row.names = F)
      
    } # end for nData
    
  } # end for nModel
  
} # end for i
  


  