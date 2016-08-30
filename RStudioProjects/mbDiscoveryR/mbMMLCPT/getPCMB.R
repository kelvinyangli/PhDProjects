# a function to read pcmb results from csv file 
# the csv file is outputed from c++ program of pcmb
# 

getPCMB = function(fileName) {
  
  mbList = list()
  
  res = read.csv(fileName, header = FALSE)
  
  res = as.matrix(res[, -(1:4)])
  
  colnames(res) = c()
  
  for (i in 1:nrow(res)) {
    
    #pcIndex = which(res[i, ] == "pc:")
    indexStart = which(res[i, ] == "mb:") + 1
    
    if (sum(res[i, ] == "") > 0) {
      
      indexEnd = min(which(res[i, ] == "")) - 1
      
    } else {
      
      indexEnd = ncol(res)
      
    } # end else
    
    
    if (indexEnd < indexStart) { # if the learned mb is empty then return empty vector
      
      mbList[[i]] = c()
      
    } else { # else return the identified candidates
      
      mbList[[i]] = as.numeric(strsplit(res[i, (indexStart:indexEnd)], split = ",")) + 1
      
    } # end else 
    
  } # end for i
  
  #names(mbList) = colnames(alarm)
  
  return(mbList)
  
}