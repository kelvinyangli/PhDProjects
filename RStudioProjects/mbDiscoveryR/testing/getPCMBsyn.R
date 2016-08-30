# evaluate pcmb and iamb results that saved from system call on c++ 
# method = iamb is 0, pcmb is 1, same as pcmb in c++
# this function returns a list containing the indices of the learned candidates with NULL represents no mb 
 
getPCMBsyn = function(results, method) {
    
  mbList = list()
  
  for (i in 2:length(results)) {
    
    if (method == 0) {
      
      temp = strsplit(results[i], split = "mb:")[[1]][2] # remove string before mb results
      temp = strsplit(temp, split = " nodes")[[1]][1] # remove string after mb results
      
      if (is.na(temp)) {
        
        temp = c()
        
      } else {
        
        temp = as.numeric(strsplit(temp, split = ",")[[1]]) + 1 # convert to numerical values
        
      } # end else 
      
    } else if (method == 1) {
      
      temp = strsplit(results[i], split = "mb:")[[1]][2] # remove string before mb results
      
      if (is.na(temp)) {
        
        temp = c()
        
      } else {
        
        temp = as.numeric(strsplit(temp, split = ",")[[1]]) + 1 # convert to numerical values
        
      } # end else 
      
    } # end else 
    
    mbList[[i - 1]] = temp 
    
  } # end for i
  
  return(mbList)
  
}