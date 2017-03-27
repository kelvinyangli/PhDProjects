# this script format the optimal local polytree structures learned into the
# required format for camml prior
fixed = TRUE
#nVars = 20
<<<<<<< HEAD
n = 100
=======
n = 1000
>>>>>>> master
dir = "../../../UAI_exp/"
mbptsList = list()
for (i in 1:8) mbptsList[[i]] = readRDS(paste0("MBPTs/", i - 1, ".rds")) 

# log factorial sheet
logFactorialSheet = read.csv("logFactorial_1to10000.csv")

data_sets = list.files(paste0(dir, "data_csv/", n, "/"))
maxMB = 7
nResamples = 5
maxProb = 0.8
#Rprof("prior.out")
for (i in 1:length(data_sets)) {
#for (i in 2:2) {
  filename = strsplit(data_sets[i], ".csv")[[1]][1]
  data = read.csv(paste0(dir, "data_csv/", n, "/", data_sets[i]))
  vars = colnames(data)
  mbList = readRDS(paste0(dir, "mb/",  n, "/", filename, ".rds"))
  for (j in 1:length(mbList)) {
    
    if (length(mbList[[j]]) > maxMB) mbList[[j]] = mbList[[j]][1:maxMB]
    
  }
  
  localStrs = readRDS(paste0(dir, "local_pt/",  n, "/", filename, ".rds"))
  
  mtx = mergeMBPTs(localStrs, vars) # merge local pts
  arcs = extractArcs(mtx) # get arcs
  if (fixed) {
    priors = list(directed = rep(maxProb, length(arcs$directed)/2), undirected = rep(0.8, length(arcs$undirected)/2))
  } else {
    priors = arcPrior(arcs, maxProb, mbList, localStrs, mbptsList, vars, data, nResamples) # sample priors
  }
  text = cammlPrior(arcs, priors)
  if (fixed) {
    write_file(text, paste0(dir, "prior_fixed/", n, "/", filename, ".txt"))
  } else {
    write_file(text, paste0(dir, "prior_unfixed/", n, "/", filename, ".txt"))
  }
  
}
#Rprof(NULL)
#proftable("prior.out")









