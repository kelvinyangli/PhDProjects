# method is either cpt std or cpt sym
# this is for synthetic models
mbMMLCPT = function(model, n, nIter, method) {
  
  # apply mmlCPT 
  datasets = list.files(paste0(model, "/data/"), pattern = paste0("_", n, "_"))
  models = list.files(paste0(model, "/cpts/"))
  
  taskID = Sys.getenv("SLURM_ARRAY_TASK_ID")
  fileIndex = strtoi(taskID)
    
  data = read.table(paste0(model, "/data/", datasets[fileIndex]))
  cpts = readRDS(paste0(model, "/cpts/", models[ceiling(fileIndex / nIter)])) # load model to get allNodes for parsePCMB
  colnames(data) = names(cpts)
  allNodes = names(cpts)
  
  # prepare for mmlCPT
  dataInfo = getDataInfo(data) 
  mbList = list()
  
  # compute mb of each node using standard forward selection
  for (i in 1:length(allNodes)) {
    
    targetNode = allNodes[i]
    mbList[[i]] = mbForwardSelection.fast(data, targetNode, mmlCPT.fast, dataInfo$arities, dataInfo$indexListPerNodePerValue)
    
  } # end for i 
  
  saveRDS(mbList, paste0(model, "/mb/", method, "/", datasets[fileIndex], ".rds")) # save mbList into folder
  
  # use symmetry condition to re-check for mb candidate for each node
  mbList = symmetryCheck(allNodes, mbList)
  
  saveRDS(mbList, paste0(model, "/mb/", method, "/", datasets[fileIndex], ".rds")) # save mbList into folder
  
}


