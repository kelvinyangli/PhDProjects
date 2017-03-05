# 7. using bootstrapping to measure the uncertainty of a feature in a local str
str_resampled = list()
r = 100
for (i in 1:r) {
  
  indices = sample(1:n, n, replace = TRUE)
  data_resampled = data[indices, ]
  dataInfo_resampled = getDataInfo(data_resampled)
  mmlmtx = computeMMLMatrix(vars, mbList[[2]], vars[2], dataInfo_resampled, n)
  scores = rep(0, length(mbpts))
  for (j in 1:length(mbpts)) scores[j] = mmlDag_fast(mbpts[[j]], vars, dataInfo_resampled, mmlmtx, n)
  index = which.min(scores)
  str_resampled[[i]] = mbpts[[index]]
  
}

g(str_resampled, "V2", "V9")

g = function(str_resampled, x, y) {
  
  u = v = w = 0 
  
  for (k in 1:length(str_resampled)) {
    
    if (str_resampled[[k]][x, y] == 1) {
      u = u + 1
    } else if (str_resampled[[k]][y, x] == 1) {
      v = v + 1
    } else {
      w = w + 1
    }
    
  } # end for k
  
  df = data.frame(u, v, w)
  colnames(df) = c(paste0(x, "->", y), paste0(x, "<-", y), paste0(x, "..", y))
  return(df)
  
}