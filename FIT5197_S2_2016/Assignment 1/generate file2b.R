# create file2b 
file2a = read.csv("Assignment 1/file2a.csv")
file2b = matrix(data = 0, nrow = nrow(file2a), ncol = 13)

# convert variables 1 to 11 to binomial variables
for (j in 1:11) {
  
  for (i in 1:nrow(file2a)) {
    
    if (file2a[i, j] <= 0 || is.na(file2a[i, j])) {
      
      file2b[i, j] = 0
      
    } else {
      
      file2b[i, j] = 1
      
    } # end else 
    
  } # end for i
  
} # end for j

# convert variables 12 to 13 to trinomial variables
for (j in 12:13) {
  
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

# keep the rest of the variables as what they are in file2a
file2b = cbind(file2b, file2a[, 14:17])

# change column names
colnames(file2b) = colnames(file2a)

# write file2b to a csv file 
write.csv(file2b, "Assignment 1/file2b.csv", row.names = FALSE)



