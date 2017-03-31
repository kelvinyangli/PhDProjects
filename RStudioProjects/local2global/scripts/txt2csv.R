models = c("alarm3", "insurance3", "child3")
n = c(500, 1000, 5000)
for (m in 1:length(models)) {
  
  for (s in 1:length(n)) {
    
    files = list.files(paste0("../../../UAI_exp/", models[m], "_data/"), paste0("_s", n[s], "_"))
    for (i in 1:length(files)) {
      
      x = paste0("../../../UAI_exp/", models[m], "_data/", files[i])
      filename = strsplit(files[i], ".txt")[[1]][1]
      directory = paste0("../../../UAI_exp/", models[m], "/data_csv/", n[s], "/", filename, ".csv")
      txt2csv(x, directory)
      
    }
    
  }
  
}
