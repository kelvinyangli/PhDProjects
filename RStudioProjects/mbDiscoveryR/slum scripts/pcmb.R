# method is 1 (pcmb) or 0 (iamb)

pcmb = function(model, n, nIter, method) {
  
  if (method == "pcmb") {
    
    methodIndex = 1
    
  } else if (method == "iamb") {
    
    methodIndex = 0 
    
  }
  
  datasets = list.files(paste0(model, "/data/"), pattern = paste0("_", n, "_"))
  models = list.files(paste0(model, "/cpts/"))
  
  taskID = Sys.getenv("SLURM_ARRAY_TASK_ID")
  fileIndex = strtoi(taskID)
    
  cpts = readRDS(paste0(model, "/cpts/", models[ceiling(fileIndex / nIter)]))
  
  allNodes = names(cpts)
  
  file.copy(paste0(model, "/data/", datasets[fileIndex]), paste0(model, ".data"), overwrite = TRUE) # copy data from "alarm data" to "pcmb" with new name "alarm.data"
  # notice that since data is copied to the same directory with the same name, they will be replaced by each other
  # also since we don't order datasets in "alarm data" folder, so the order of results is different from order of results in mmlcpt
  # but that's not a problem, since we only consider average, but not individual result
  
  results = system(paste0("kmb4 ", model, ".data ", n, " ", length(allNodes), " -1 1.0 1 ", methodIndex, " 0.01"), intern = TRUE)
  
  output = read.table("output.txt")[, 1] # load output file from c++
  
  mbList = parsePCMB(output, length(allNodes), allNodes)
  
  saveRDS(mbList, paste0(model, "/mb/", method, "/", datasets[fileIndex], ".rds")) # save learned mb as .rds
  
  file.remove("output.txt")
  
}
