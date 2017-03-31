(files = list.files("../../../UAI_exp/child5_data/", "_s500_"))
for (i in 1:10) {
  
  x = paste0("../../../UAI_exp/child5_data/", files[i])
  filename = strsplit(files[i], ".txt")[[1]][1]
  directory = paste0("../../../UAI_exp/child5/data_csv/500/", filename, ".csv")
  txt2csv(x, directory)
  
}