# scripts evaluate relief results
# convert txt to mbList in rds
model = "alarm"
cpts = read.dsc("alarm/cpts/alarm.dsc")
files = list.files(paste0(model, "/mb/relief txt"))
for (i in 1:length(files)) {
  
  result = read.table(paste0(model, "/mb/relief txt/", files[i]))
  colnames(result) = NULL
  rownames(result) = NULL
  
  mbList = parseRelief(result, cpts)
  saveRDS(mbList, paste0(model, "/mb/relief/", strsplit(files[i], ".arff.txt")[[1]], ".rds"))
  
}

computeStats4(model, "relief", 100)
computeStats4(model, "relief", 500)
computeStats4(model, "relief", 1000)
computeStats4(model, "relief", 10000)