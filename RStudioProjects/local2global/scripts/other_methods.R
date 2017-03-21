# mmhc and chow.liu 
dir = "../../../UAI_exp/"
methods = c("mmhc_bnlearn", "tabu_bde_bnlearn", "chow_liu")
n = c(100, 1000)
# list all datasets in dir/data folder
for (k in 1:length(n)) {
  
  datasets = list.files(paste0(dir, "data_csv/", n[k], "/"))
  for (nData in 1:length(datasets)) {
    
    filename = strsplit(datasets[nData], ".csv")[[1]][1]
    modelName = paste(strsplit(filename, "_")[[1]][1:5], collapse = "_")
    cpts = readRDS(paste0(dir, "dag/", modelName, ".rds"))$cpts
    data = read.csv(paste0(dir, "data_csv/", n[k], "/", datasets[nData]))
    nl = varLevels(cpts)
    for (j in 1:ncol(data)) {
      
      if (nlevels(data[, j]) != nl[j]) levels(data[, j]) = LETTERS[1:nl[j]]
      
    }
    
    for (nMethods in 1:length(methods)) {
      
      if (methods[nMethods] == "mmhc_bnlearn") {
        
        learned = mmhc(data, score = "bde")
        
      } else if (methods[nMethods] == "tabu_bde_bnlearn") {
        
        learned = tabu(data, score = "bde")
        
      } else if (methods[nMethods] == "chow_liu") {
        
        learned = chow.liu(data)
        
      } # end else 
      
      dir.create(paste0(dir, methods[nMethods], "/", n[k]), showWarnings = TRUE)
      saveRDS(learned, paste0(dir, methods[nMethods], "/", n[k], "/", filename, ".rds"))
      
    } # end nMethods
  
  } # end nData
  
} # end k 


