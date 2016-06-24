# tetrad pdag matrix to standard pdag matrix contains only 1 and 0 for the use of matlab
# 
tetrad2Std = function(currentDirectory) {
  
  directory = paste0(currentDirectory, "/Learned networks/pc_pattern/")
    
  tetradMatrices = list.files(directory)
    
  datasets = list.files(paste0(currentDirectory, "/Datasets/Training csv/"))
    
  for (k in 1:length(tetradMatrices)) {
    
    tetrad = read.table(paste0(directory, tetradMatrices[k]))
    
    data = read.csv(paste0(currentDirectory, "/Datasets/Training csv/", datasets[k]))
      
    colnames(tetrad) = colnames(data)
    
    dagMatrix = matrix(NA, nrow = nrow(tetrad), ncol = ncol(tetrad))
    
    colnames(dagMatrix) = colnames(data)
    
    # null --- 0
    # [i, j] = Arrow && [j, i] = Tail --- [i, j] = 1 & [j, i] = 0
    # undirected or bidirected --- [i, j] = [j, i] = 1

    for (i in 1:nrow(tetrad)) {
      
      for (j in i:ncol(tetrad)) {
        
        if ((tetrad[i, j] == "Arrow") && (tetrad[j, i] == "Tail")) {
          
          dagMatrix[i, j] = 1
          dagMatrix[j, i] = 0
          
        } else if ((tetrad[i, j] == "Tail") && (tetrad[j, i] == "Arrow")) {
          
          dagMatrix[i, j] = 0
          dagMatrix[j, i] = 1
          
        } else if ((tetrad[i, j] == "Tail") && (tetrad[j, i] == "Tail")) {
          
          dagMatrix[i, j] = 1
          dagMatrix[j, i] = 1
          
        } else if ((tetrad[i, j] == "Arrow") && (tetrad[j, i] == "Arrow")) {
          
          dagMatrix[i, j] = 1
          dagMatrix[j, i] = 1
          print("bidirected!")
          
        } else if ((tetrad[i, j] == "Null") && (tetrad[j, i] == "Null")) {
          
          dagMatrix[i, j] = 0
          dagMatrix[j, i] = 0
          
        } 
        
      } # end for j
      
    } # end for i
    
    filename = tetradMatrices[k]
    filename = strsplit(filename, split = "\\.")[[1]][c(1,3)]
    filename = paste0(filename, collapse = ".")
    
    write.csv(dagMatrix, paste0(currentDirectory, "/Learned networks/dagMatrix/pdag_pc/", filename), row.names = FALSE)
    
  } # end for k

  
}



