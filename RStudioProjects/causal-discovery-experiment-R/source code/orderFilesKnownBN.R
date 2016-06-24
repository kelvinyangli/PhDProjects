# order files for known BNs
orderFilesKnownBN = function(files, numIterations) {
  numbersInName = matrix(0, nrow = length(files), ncol = 2)
  for (i in 1:length(files)) {
    numbersInName[i,] = unique(na.omit(as.numeric(unlist(strsplit(files[i], "[^0-9]+")))))
  }
  firstOrder = order(numbersInName[,1])
  numbersInName = numbersInName[firstOrder,]
  orderedFiles = files[firstOrder]
  
  for (j in 1:(length(files)/numIterations)) {
    secondOrder = order(numbersInName[((j-1)*numIterations+1):(j*numIterations), 2])
    orderedFiles[((j-1)*numIterations+1):(j*numIterations)] = orderedFiles[((j-1)*numIterations+1):(j*numIterations)][secondOrder]
  }
  return(orderedFiles)
}