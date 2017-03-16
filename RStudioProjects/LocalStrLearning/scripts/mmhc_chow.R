# mmhc and chow.liu 
dir = "../../../Dag experiments/"
method = "mmhc_bnstruct"
# list all datasets in dir/data folder
datasets = list.files(paste0(dir, "data/"))
for (nData in 1:length(datasets)) {

  data = readRDS(paste0(dir, "data/", datasets[nData]))
  
  if (method == "mmhc_bnstruct") {
    
    vars = colnames(data)
    m = length(vars)
    v = rep(0, m)
    for (i in 1:m) v[i] = nlevels(data[,i])
    data_bnstruct = BNDataset(data = string2numeric(data), discreteness = rep(TRUE, m), variables = vars, node.size = v)
    learned = learn.network(data_bnstruct, algo = "mmhc", scoring.func = "BDeu", alpha = 0.05, ess = 10)@dag
    dimnames(learned) = list(vars, vars)
    
  } else if (method == "mmhc") {
    
    learned = mmhc(data, score = "bde")
    
  } else if (method == "bde") {
    
    learned = tabu(data, score = "bde")
    
  } else if (method == "chow") {
    
    learned = chow.liu(data)
    
  } 
  
  saveRDS(learned, paste0(dir, method, "/", datasets[nData]))
  
}

