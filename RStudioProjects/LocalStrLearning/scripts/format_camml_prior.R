# this script format the optimal local polytree structures learned into the
# required format for camml prior

dir = "../../../Dag experiments/"
mbptsList = list()
for (i in 1:8) mbptsList[[i]] = readRDS(paste0("MBPTs/", i - 1, ".rds")) 

# log factorial sheet
logFactorialSheet = read.csv("logFactorial_1to10000.csv")

data_sets = list.files(paste0(dir, "data/"), "20_")
nResamples = 5
maxProb = 0.8

for (i in 10:length(data_sets)) {
  
  data = readRDS(paste0(dir, "data/", data_sets[i]))
  vars = colnames(data)
  mbList = readRDS(paste0(dir, "mb/", data_sets[i]))
  localStrs = readRDS(paste0(dir, "local_pt/", data_sets[i]))
  
  mtx = mergeMBPTs(localStrs, vars) # merge local pts
  arcs = extractArcs(mtx) # get arcs
  priors = arcPrior(arcs, maxProb, mbList, localStrs, mbptsList, vars, data, nResamples) # sample priors
  text = cammlPrior(arcs, priors)
  filename = strsplit(data_sets[i], ".rds")[[1]][1]
  write_file(text, paste0(dir, "prior_unfixed/", filename, ".txt"))
  
}










