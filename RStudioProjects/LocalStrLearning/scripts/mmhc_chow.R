# mmhc and chow.liu 
dir = "../../../Dag experiments/"

# list all datasets in dir/data folder
datasets = list.files(paste0(dir, "data/"))
for (nData in 1:length(datasets)) {
  
  data = readRDS(paste0(dir, "data/", datasets[nData]))
  #learned = chow.liu(data)
  #saveRDS(learned, paste0(dir, "pt_chow/", datasets[nData]))
  #learned = mmhc(data, score = "bde")
  learned = tabu(data, score = "bde")
  saveRDS(learned, paste0(dir, "tabu_bde/", datasets[nData]))
  
}

