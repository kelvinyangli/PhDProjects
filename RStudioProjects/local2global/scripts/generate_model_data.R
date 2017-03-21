# generating random polytree adjmtx and cpts at the given specification
# sampling data from polytree models for given sample sizes 
nVars = 30
maxNPas = 3
maxArity = 4
beta = c(1, 10)
n = c(100, 1000)
nRepeat = 5 # the number of times repeat this experiment
folder = "UAI_exp/"
dir = paste0("../../../", folder)

for (nModel in 1:nRepeat) {
  
  model_seed_1 = randSeed()
  set.seed(model_seed_1)
  adjmtx = randAdjmtx(nVars, maxNPas)
  dag = matrix2dag(adjmtx) # adjmtx -> bnlearn format
  
  cpts_1 = randCPTs(dag, maxArity, beta[1])
  filename_1 = paste(nVars, maxNPas, maxArity, beta[1], model_seed_1, sep = "_")
  ls_1 = list("adjmtx" = adjmtx, "cpts" = cpts_1) # combine adjmtx and cpts into a list
  
  model_seed_10 = randSeed()
  set.seed(model_seed_10)
  cpts_10 = randCPTs(dag, maxArity, beta[2])
  filename_10 = paste(nVars, maxNPas, maxArity, beta[2], model_seed_10, sep = "_")
  ls_10 = list("adjmtx" = adjmtx, "cpts" = cpts_10) # combine adjmtx and cpts into a list

  saveRDS(ls_1, paste0(dir, "/dag/", filename_1, ".rds")) # save list 
  saveRDS(ls_10, paste0(dir, "/dag/", filename_10, ".rds")) # save list 
  
}


models = list.files(paste0(dir, "dag/"), paste0(nVars, "_"))
for (i in 1:length(models)) {
  
  cpts = readRDS(paste0(dir, "dag/", models[i]))$cpts
  
  filename = strsplit(models[i], ".rds")[[1]][1]
  
  for (nData in 1:nRepeat) {
    
    data_seed_100 = randSeed()
    set.seed(data_seed_100)
    data_100 = rbn(cpts, n[1]) # sample 1k samples
    name_100 = paste(filename, n[1], data_seed_100, sep = "_")
    
    data_seed_1000 = randSeed()
    set.seed(data_seed_1000)
    data_1000 = rbn(cpts, n[2])
    name_1000 = paste(filename, n[2], data_seed_1000, sep = "_")
    
    write.csv(data_100, paste0(dir, "data_csv/100/", name_100, ".csv"), row.names = F)
    write.csv(data_1000, paste0(dir, "data_csv/1000/", name_1000, ".csv"), row.names = F)
    
  } # end for nData
  
}



  