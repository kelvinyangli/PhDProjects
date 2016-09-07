# evaluate pcmb and iamb results that saved from system call on c++ 
# method = iamb is 0, pcmb is 1, same as pcmb in c++
# this function returns a list containing the indices of the learned candidates with NULL represents no mb 
# if no mb is learned then return -1 in mbList to avoid list having lenght 0

getPCMBsyn = function(results, method) {
    
  mbList = list()
  
  for (i in 2:length(results)) {
    
    if (method == 0) { # for iamb
      
      temp = strsplit(results[i], split = "mb:")[[1]][2] # remove string before mb results
      temp = as.numeric(strsplit(temp, split = " nodes")[[1]][1]) # remove string after mb results
      
      if (is.na(temp)) {
        
        temp = -1
        
      } else {
        
        temp = as.numeric(strsplit(temp, split = ",")[[1]]) + 1 # convert to numerical values
        
      } # end else 
      
    } else if (method == 1) { # for pcmb
      
      temp = strsplit(results[i], split = "mb:")[[1]][2] # remove string before mb results
      
      if (is.na(temp)) {
        
        temp = -1
        
      } else {
        
        temp = as.numeric(strsplit(temp, split = ",")[[1]]) + 1 # convert to numerical values
        
      } # end else 
      
    } # end else 
    
    mbList[[i - 1]] = temp 
    
  } # end for i
  
  #mbList[length(results)] = NULL
  
  return(mbList)
  
}

parsePCMB = function(output, nNodes, allNodes) {
  
  mbList = list()
  
  for (i in 1:nNodes) {
    
    temp = unique(output[((i - 1) * nNodes + 1):(i * nNodes)])
    temp = temp[temp != -1] # remove -1 from temp
    
    
    if (length(temp) < 1) {
      
      #temp = -1 # if no mb is found then return -1 to avoid confusion
      mbList[[i]] = as.character(c())
      
    } else {
      
      mbList[[i]] = allNodes[temp + 1]
      
    } # end else 
    
  } # end for i
  
  return(mbList)
  
}




