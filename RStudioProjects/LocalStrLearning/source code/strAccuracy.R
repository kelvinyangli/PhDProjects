# this function compares structures b/w two (local) model strs 
# it returns add, delete and reverse 
strAccuracy = function(true, learned) {
  
  # add is in true but not learned 
  # delete is in learned but not true
  # reverse is in both but wrong direction
  add = delete = reverse = 0 
  
  for (i in 1:nrow(true)) {
    
    for (j in 1:ncol(true)) {
      
      if ((true[i, j] == 1) && (learned[i, j] == 0) && (learned[j, i] == 0)) {
        
        add = add + 1
        
      } else if ((true[i, j] == 0) && (true[j, i] == 0) && (learned[i, j] == 1)) {
        
        delete = delete + 1
        
      } else if ((true[i, j] + true[j, i] == 1) && (learned[i, j] + learned[j, i] == 1) && (true[i, j] != learned[i, j])) {
        
        reverse = reverse + 0.5
        
      } # end else 
      
    } # end for j 
    
  } # end for i
  
  df = data.frame(add, delete, reverse)
  return(df)
  
}