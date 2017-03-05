# this function check if a given graph is a dag
# the input is based on graph adjacency matrix
# mtx %^% n is the power of a mtrix, this requires the library expm
isDag = function(mtx) {
  
  eligible = TRUE
  
  for (n in 1:ncol(mtx)) {
    
    if (sum(diag(mtx %^% n)) > 0) {
      
      eligible = FALSE
      break
      
    } # end if 
    
  } # end for n
  
  return(eligible)
  
}


