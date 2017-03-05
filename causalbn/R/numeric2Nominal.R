# this function converts numerical data with the type 0, 1, 2, ... to nominal data 
# by adding levels to each column and change the data type for each column from integer to factor
numeric2Nominal = function(data) {
  
  for (j in 1:ncol(data)) {
    
    levels(data[, j]) = unique(data[, j])
    data[, j] = as.factor(data[, j])
    
  }
  
  return(data)
  
}

