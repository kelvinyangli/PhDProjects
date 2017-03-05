# estimate cpts from data 
# tb is the results of table(data)
# this is used to compare results with belief propagation
estProb = function(tb) {
  
  dimension = dim(tb)
  prod = c()
  
  for (i in 1:prod(dimension[-c(1, 2)])) {
    
    n = prod(dimension[c(1, 2)])
    counts = tb[((i - 1) * n + 1):(i * n)]
    
    m = dimension[1]
    
    for (j in 1:dimension[2]) {
      
      p = counts[((j - 1) * m + 1):(j * m)] / sum(counts[((j - 1) * m + 1):(j * m)])
      prod = c(prod, p)
      
    } # end for j
    
  } # end for i
  
  prod = array(prod, dim = dimension, dimnames = dimnames(tb))
  
  return(prod)
  
}


