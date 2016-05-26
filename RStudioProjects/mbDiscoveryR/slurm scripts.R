# test for mml cpt
datasets = list.files("Datasets/")

taskid <- Sys.getenv("SLURM_ARRAY_TASK_ID") 

fileIndex = strtoi(taskid) 
  
data = readRDS(paste0("Datasets/", datasets[fileIndex]))

#cpts = readRDS(paste0("CPTs/", datasets[i]))
ls = list()

for (j in 1:ncol(data)) {
  
  # find mb of all nodes
  node = names(data)[j]
  
  #mbTrue = mb(cpts, node)
  
  ls[[names(data)[j]]] = mbMMLCPT(data, node, base = 2, debug = TRUE)
  
}

saveRDS(ls, paste0("mbMMLCPT/", datasets[fileIndex]))


  
