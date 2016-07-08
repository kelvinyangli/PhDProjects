# get arity of all variables for all data sets
currentDirectory = "alarm"

datasets = list.files(paste0(currentDirectory, "/Datasets/Testing"))

for (i in 1:length(datasets)) {
  data = readRDS(paste0(currentDirectory, "/Datasets/Testing/", datasets[i]))

  arities = rep(2, ncol(data))
  
  for (j in 1:ncol(data)) arities[j] = nlevels(data[,j])  
  
  write.csv(arities, paste0(currentDirectory, "/Datasets/Arities/", datasets[i], ".csv"), row.names = FALSE)
  
}
