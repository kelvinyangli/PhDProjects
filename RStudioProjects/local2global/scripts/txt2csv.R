files = list.files("../../../UAI_exp/hailfinder/data_csv/", "s1000")
for (i in 1:10) {
  
  x = paste0("../../../UAI_exp/hailfinder/data_csv/", files[i])
  filename = strsplit(files[i], ".txt")[[1]][1]
  directory = paste0("../../../UAI_exp/hailfinder/data_csv/1000/", filename, ".csv")
  txt2csv(x, directory)
  
}