# convert .rds to .csv 
# 
currentDirectory = "sachs"

files = list.files(paste0(currentDirectory, "/Datasets/Training/"))

orderings = list.files(paste0(currentDirectory, "/Ordering/"))

for (i in 1:560) {
  
  data = readRDS(paste0(currentDirectory, "/Datasets/Training/", files[i]))
  
  ordering = read.csv(paste0(currentDirectory, "/Ordering/", orderings[i]), header = FALSE)
  
  ordering = t(ordering)
  
  data = data[,ordering] # take ordering from MWST learned in matlab's bnt
  
  # sachs data: convert 1, 2,3 -> A, B, C
  # for (j in 1:ncol(data)) {
  
  #  data[, j] = paste0("A", data[, j])
  
  # }
  
  write.csv(data, paste0(currentDirectory, "/Datasets/Training csv/", files[i], ".csv"), row.names = FALSE)
  
}


