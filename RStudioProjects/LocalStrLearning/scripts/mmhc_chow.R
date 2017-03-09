# mmhc and chow.liu 
dir = "../../../Polytree experiments/"

# list all datasets in dir/data folder
datasets = list.files(paste0(dir, "data/"))
for (nData in 1:400) {
  
  data = readRDS(paste0(dir, "data/", datasets[nData]))
  #learned = chow.liu(data)
  #saveRDS(learned, paste0(dir, "pt_chow/", datasets[nData]))
  learned = mmhc(data, score = "bde")
  saveRDS(learned, paste0(dir, "pt_mmhc/", datasets[nData]))
  
}

