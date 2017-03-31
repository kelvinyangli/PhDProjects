dir = "../../../UAI_exp/alarm5/"
n = c(500, 1000, 5000)
for (i in 1:3) {
  
  datasets = list.files(paste0(dir, "data_csv/", n[i], "/")) 
  for (nData in 1:length(datasets)) {
    
    data = read.csv(paste0(dir, "data_csv/", n[i], "/", datasets[nData]))
    data = numeric2Nominal(data)
    for (j in 1:ncol(data)) {
      
      if (nlevels(data[, j]) == 1) levels(data[, j]) = c("1", "2")
      
    }
    learned = bnlearn::mmhc(data, score = "bde", iss = 1, alpha = 0.05)
    filename = strsplit(datasets[nData], ".csv")[[1]][1]
    saveRDS(learned, paste0(dir, "mmhc/", n[i], "/", filename, ".rds"))
    
  }
  
}
