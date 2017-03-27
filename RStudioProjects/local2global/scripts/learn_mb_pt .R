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
dir = "../../../UAI_exp/"
nVars = 20
maxNPas = 3
maxArity = 4
beta = 1
maxMB = 7 
n = 100
# read pre-saved mbpts into memory 
mbptsList = list()
for (i in 1:8) mbptsList[[i]] = readRDS(paste0("MBPTs/", i - 1, ".rds")) 

logFactorialSheet = read.csv("logFactorial_1to10000.csv") # log factorial sheet

# list all datasets in dir/data folder
datasets = list.files(paste0(dir, "data_csv/", n, "/"), paste(nVars, maxNPas, maxArity, beta, sep = "_"))
#mbLists = list.files(paste0(dir, "mb/", n, "/"))
#strLists = list.files(paste0(dir, "local_pt/", n, "/"))

for (nData in 1:length(datasets)) {
#for (nData in 9:400) {
  
  #cat(nData, "\n")
  data = read.csv(paste0(dir, "data_csv/", n, "/", datasets[nData]))
  data = numeric2Nominal(data)
  #data = read.csv(paste0(dir, "data_csv/", n, "/", datasets[nData]))
  dataInfo = getDataInfo(data)
  vars = colnames(data)
  #n = as.numeric(strsplit(datasets[nData], "_")[[1]][5]) # get sample size from file name
  
  mbList = list()
  # learn mb(x), for all x \in vars
  for (i in 1:length(vars)) {
  #  
    mbList[[i]] = mbForwardSelection.fast(data, vars[i], dataInfo$arities, 
                                          dataInfo$indexListPerNodePerValue, 
                                          base = exp(1))
  # 
  }
  
  filename = strsplit(datasets[nData], ".csv")[[1]][1]
  mbList = symmetryCorrection(vars, mbList) # apply symmetry correction 
  saveRDS(mbList, paste0(dir, "mb/", n, "/", filename, ".rds")) # save learned mb candidates 
  
  #mbList = readRDS(paste0(dir, "mb/", n, "/", mbLists[nData]))
  #strList = readRDS(paste0(dir, "local_pt/", n, "/", strLists[nData]))
  
  # restrict mb size to be <= 7 by dropping extra candidates
  for (i in 1:length(vars)) {
    
    if (length(mbList[[i]]) > maxMB) mbList[[i]] = mbList[[i]][1:maxMB]
    
  }
  
  # 4. learn local str for each var based on its learned mb 
  # 5. merging local structures into global structure
  #mbpt_global = mergeMBPTs(strList, vars)
  #mbpt_global = refineMergedMBPT(mbpt_global)  
  
  
  learned = learnMBPT(vars, mbList, mbptsList, dataInfo, n)
  #localStrs = learned$localStrs
  #mbpt_global = learned$mbpt
  saveRDS(learned$localStrs, paste0(dir, "local_pt/", n, "/", filename, ".rds"))
  saveRDS(learned$mbpt, paste0(dir, "global_pt/", n, "/", filename, ".rds"))
  
}

