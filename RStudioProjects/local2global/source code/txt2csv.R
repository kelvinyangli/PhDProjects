txt2csv = function(x, directory) {
  
  file = read.table(x)
  rownames(file) = c()
  write.csv(file, directory, row.names = FALSE)
  
} 