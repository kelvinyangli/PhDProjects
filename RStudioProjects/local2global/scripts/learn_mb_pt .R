# learning global polytree structures from data using mb results returned by 
# mbmml, then exhaustively searching for the optimal local structure within 
# each mb according to the mmlCPT, the restricted mb size is less than 8 nodes
# estimated local structures are merged into a global polytree 
#nVars = c(20, 40, 60, 80)
#maxNPas = 2:5
#maxArity = 3:6
#beta = c(1, 5, 10, 15)
#n = c(1000, 10000)
#nExp = 10 # the number of times repeat this experiment
#nVars = 50
#maxNPas = 4
#maxArity = 4
#beta = 1
mbptsList = list()
for (i in 1:8) mbptsList[[i]] = readRDS(paste0("MBPTs/", i - 1, ".rds"))  # read pre-saved mbpts into memory 
logFactorialSheet = read.csv("logFactorial_1to10000.csv") # log factorial sheet
real = TRUE
maxMB = 7
models = c("child")
n = c(5000)
for (m in 1:length(models)) {
  dir = paste0("../../../UAI_exp/", models[m], "/")
  for (s in 1:length(n)) {
    cat(models[m], "-", n[s], "\n")
    # list all datasets in dir/data folder
    if (real) {
      datasets = list.files(paste0(dir, "data_csv/", n[s], "/")) 
    } else {
      datasets = list.files(paste0(dir, "data_csv/", n[s], "/"), paste(nVars, maxNPas, maxArity, beta, sep = "_"))
    }
    
    for (nData in 1:length(datasets)) {
      
      data = read.csv(paste0(dir, "data_csv/", n[s], "/", datasets[nData]))
      if (real) data = numeric2Nominal(data)
      #data = read.csv(paste0(dir, "data_csv/", n, "/", datasets[nData]))
      dataInfo = getDataInfo(data)
      vars = colnames(data)
      # learn mb
      mbList = list()
      for (i in 1:length(vars)) mbList[[i]] = mbForwardSelection.fast(data, vars[i], dataInfo$arities, dataInfo$indexListPerNodePerValue, base = exp(1))
      filename = strsplit(datasets[nData], ".csv")[[1]][1]
      mbList = symmetryCorrection(vars, mbList) # apply symmetry correction 
      saveRDS(mbList, paste0(dir, "mb/", n[s], "/", filename, ".rds")) # save learned mb candidates 
      # restrict mb size to be <= 7 by dropping extra candidates
      for (i in 1:length(vars)) if (length(mbList[[i]]) > maxMB) mbList[[i]] = mbList[[i]][1:maxMB]
      # local-to-global
      learned = learnMBPT(vars, mbList, mbptsList, dataInfo, n[s])
      saveRDS(learned$localStrs, paste0(dir, "local_pt/", n[s], "/", filename, ".rds"))
      saveRDS(learned$mbpt, paste0(dir, "global_pt/", n[s], "/", filename, ".rds"))
      
    } # end for nData
  } # end for s
  
} # end for m 


