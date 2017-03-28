files = list.files("../../../UAI_exp/barley_data/", "s500")
for (i in 1:10) {
  
  x = paste0("../../../UAI_exp/barley_data/", files[i])
  filename = strsplit(files[i], ".txt")[[1]][1]
  directory = paste0("../../../UAI_exp/barley/data_csv/500/", filename, ".csv")
  txt2csv(x, directory)
  
}