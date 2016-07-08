# convert dag matrix to dag and cpts in bnlearn
# dag matrix maybe learned by k2 (matlab) or pcPatter (tetrad)

currentDirectory = "asia"

matrices = list.files(paste0(currentDirectory, "/Learned networks/dagMatrix/k2Matlab/"))
datasets = list.files(paste0(currentDirectory, "/Datasets/Training/"))

for (i in 1:length(datasets)) {
  
  dagMatrix = read.csv(paste0(currentDirectory, "/Learned networks/dagMatrix/k2Matlab/", matrices[i]))
  
  rownames(dagMatrix) = colnames(dagMatrix)
  
  dagBN = matrix2dag(dagMatrix) # matrix to dag
  
  dataTraining = readRDS(paste0(currentDirectory, "/Datasets/Training/", datasets[i]))
  
  cptsBN = bn.fit(dagBN, dataTraining, method = "bayes")
  
  # save structure and cpts
    
  write.dot(paste0(currentDirectory, "/Learned networks/Structures/pc/", datasets[i], ".dot"), dagBN)
  
  saveRDS(cptsBN, paste0(currentDirectory, "/Learned networks/CPTs/pc/", datasets[i]))  
  
}



for (i in 1:length(cpts)) {
  cat(nrow(cpts[[i]]$prob), "\n")
}