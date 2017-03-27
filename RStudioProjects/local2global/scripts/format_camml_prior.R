# this script format the optimal local polytree structures learned into the
# required format for camml prior
fixed = FALSE
n = 5000
dir = "../../../UAI_exp/hailfinder/"
mbptsList = list()
for (i in 1:8) mbptsList[[i]] = readRDS(paste0("MBPTs/", i - 1, ".rds")) 
logFactorialSheet = read.csv("logFactorial_1to10000.csv") # log factorial sheet
datasets = list.files(paste0(dir, "data_csv/", n, "/"))
maxMB = 7
nResamples = 5
maxProb = 1
#p = seq(0.1, 0.7, 0.1)
#Rprof("prior.out")
for (i in 1:10) {
  cat("Data", i, "\n")
#for (i in 2:2) {
  filename = strsplit(datasets[i], ".csv")[[1]][1]
  data = read.csv(paste0(dir, "data_csv/", n, "/", datasets[i]))
  data = numeric2Nominal(data)
  vars = colnames(data)
  mbList = readRDS(paste0(dir, "mb/",  n, "/", filename, ".rds"))
  for (j in 1:length(mbList)) {
    
    if (length(mbList[[j]]) > maxMB) mbList[[j]] = mbList[[j]][1:maxMB]
    
  }
  
  localStrs = readRDS(paste0(dir, "local_pt/",  n, "/", filename, ".rds"))
  
  mtx = mergeMBPTs(localStrs, vars) # merge local pts
  arcs = extractArcs(mtx) # get arcs
  arcs = rbind(arcs$directed, arcs$undirected)
  if (fixed) {
    priors = list(directed = rep(maxProb, length(arcs$directed)/2), undirected = rep(0.8, length(arcs$undirected)/2))
  } else {
    count = arcCount(arcs, mbList, mbptsList, vars, data, n, nResamples) # sample priors
  }
  priors = count2Prior(count, maxProb)
  text = cammlPrior(arcs, priors)
  if (fixed) {
    write_file(text, paste0(dir, "prior_fixed/", n, "/", filename, ".txt"))
  } else {
    write_file(text, paste0(dir, "prior_unfixed/", n, "/", filename, ".txt"))
  }
  
}
#Rprof(NULL)
#proftable("prior.out")








