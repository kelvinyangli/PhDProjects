# scripts evaluate relief results
# convert txt to mbList in rds
model = "34_4_4_1"
#datasets = list.files(paste0(model, "/data training rds/"), ".rds")
#for (i in 1:length(datasets)) {
#  data = readRDS(paste0(model, "/data training rds/", datasets[i]))
#  write.arff(data, paste0(model, "/data arff/", strsplit(datasets[i], ".rds")[[1]], ".arff"))
#}

#cpts = read.dsc("alarm/cpts/alarm.dsc")
files = list.files(paste0(model, "/mb/relief txt"))
models = list.files(paste0(model, "/cpts/"), ".rds")
for (i in 1:length(files)) {
  
  result = read.table(paste0(model, "/mb/relief txt/", files[i]))
  colnames(result) = NULL
  rownames(result) = NULL
  
  cpts = readRDS(paste0(model, "/cpts/", models[ceiling(i / 20)]))
  
  mbList = parseRelief(result, cpts)
  saveRDS(mbList, paste0(model, "/mb/relief/", strsplit(files[i], ".arff.txt")[[1]], ".rds"))
  
}

computeStats(model, "relief", 100, 5)
computeStats(model, "relief", 500, 5)
computeStats(model, "relief", 1000, 5)
computeStats(model, "relief", 10000, 5)

