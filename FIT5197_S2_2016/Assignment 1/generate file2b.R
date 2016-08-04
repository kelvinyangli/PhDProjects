# create file2b 
file2a = read.csv("file2a.csv")
file2b = matrix(data = 0, nrow = nrow(file2a), ncol = ncol(file2a))

# convert column 1 to 9
for (j in 1:9) {
  
  for (i in 1:nrow(file2a)) {
    
    if (file2a[i, j] < 0 || is.na(file2a[i, j])) {
      
      file2b[i, j] = 0
      
    } else {
      
      file2b[i, j] = 1
      
    } # end else 
    
  } # end for i
  
} # end for j

# convert column 10 to 17
for (j in 10:ncol(file2a)) {
  
  for (i in 1:nrow(file2a)) {
    
    if (is.na(file2a[i, j])) {
      
      file2b[i, j] = 0
      
    } else if (file2a[i, j] <= 0) {
      
      file2b[i, j] = 1
      
    } else {
      
      file2b[i, j] = 2
      
    } # end else
    
  } # end for i
  
} # end for j

# write file2b to a csv file 
write.csv(file2b, "file2b.csv", row.names = FALSE)