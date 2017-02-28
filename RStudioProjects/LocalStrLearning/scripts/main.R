# this scripts conduct the following processes: 
# 1. generate random polytrees with given number of vars and max number of parents
# 2. generate random parameter values with give maximum arity and concentration parameter
# 3. learn mb for each var using mmlCPT with symmtry correction 
# 4. apply exhaustive polytree search within each mb according to mmlCPT
# 5. merge all learned local structures into a global structure, if an arc is bidirected 
#    then we make it undirected, if an arc is both unexisted and directed then we drop
#    the entire arc
# 6. compute edit distances for cpdags, skeletons, and dags for our mml method, 
#    mmhc, chow.liu and arance (notice the last two methods only return undirected polytrees)
# 7. use bootstrap to measure arc uncertainty (this step is optional, need to be further 
#    confirmed for its accuracy)

# 1. generate random polytree structure
adjmtx = randPolytree(20, 3)
pt = matrix2dag(adjmtx)
graphviz.plot(pt, main = "true")

# 2. generate random parameter values
cpts = randCPTs(pt, 4, 1)
n = 100000
data = rbn(cpts, n) 
dataInfo = getDataInfo(data)
vars = colnames(adjmtx)

# 3. learn mb using mmlCPT
mbList = list()
# learn mb(x), for all x \in vars
for (i in 1:length(vars)) mbList[[i]] = mbForwardSelection.fast(data, vars[i], dataInfo$arities, dataInfo$indexListPerNodePerValue)
mbList = symmetryCorrection(vars, mbList) # apply symmetry correction 

# 4. learn local str for each var based on its learned mb 
strList = list()
for (i in 1:length(vars)) {
  
  mbpts = readRDS(paste0("MBPTs/", length(mbList[[i]]), ".rds")) # load pre-saved mbpts for mb size n
  mbpts = substituteVar(mbpts, vars[i], mbList[[i]]) # replace default vars with mb vars
  mmlmtx = computeMMLMatrix(vars, mbList[[i]], vars[i], dataInfo, n) # compute mmlcpt for each node in mbVars given its possible parents
  scores = rep(0, length(mbpts)) # compute mmlcpt for each mbpt 
  for (j in 1:length(mbpts)) scores[j] = mmlDag_fast(mbpts[[j]], vars, dataInfo, mmlmtx, n)
  index = which.min(scores) # find the minimum score's index
  strList[[i]] = mbpts[[index]] # the learned mbpt
  
}

# 5. merging local structures into global structure
(mbpt_global = mergeMBPTs(strList, vars))
(mbpt_global = refineMergedMBPT(mbpt_global))

# 6. edit distance 
x = c(shd(matrix2dag(mbpt_global), pt), shd(mmhc(data), pt), shd(chow.liu(data), pt), shd(aracne(data), pt)) # cpdags
y = c(hamming(matrix2dag(mbpt_global), pt), hamming(mmhc(data), pt), hamming(chow.liu(data), pt), hamming(aracne(data), pt)) # skeletons
z = c(editDistDags(matrix2dag(mbpt_global), pt), editDistDags(mmhc(data), pt), editDistDags(chow.liu(data), pt), editDistDags(aracne(data), pt)) # dags
data.frame("cpdags" = x, "skeletons" = y, "dags" = z, row.names = c("mml", "mmhc", "chow.liu", "arance"))

# greedy search for better global str
mmlDag(mbpt_global, vars, dataInfo, n)
mmlDag(adjmtx, vars, dataInfo, n)

# ploting
graphviz.plot(matrix2dag(mbpt_global), main = "mml")
graphviz.plot(mmhc(data), main = "mmhc")
graphviz.plot(chow.liu(data), main = "chow.liu")
#graphviz.plot(aracne(data), main = "aracne")

##########################################################################################
# 7. using bootstrapping to measure the uncertainty of a feature in a local str
str_resampled = list()
r = 100
for (i in 1:r) {
  
  indices = sample(1:n, n, replace = TRUE)
  data_resampled = data[indices, ]
  dataInfo_resampled = getDataInfo(data_resampled)
  mmlmtx = computeMMLMatrix(vars, mbList[[2]], vars[2], dataInfo_resampled, n)
  scores = rep(0, length(mbpts))
  for (j in 1:length(mbpts)) scores[j] = mmlDag_fast(mbpts[[j]], vars, dataInfo_resampled, mmlmtx, n)
  index = which.min(scores)
  str_resampled[[i]] = mbpts[[index]]
  
}

g(str_resampled, "V2", "V9")

g = function(str_resampled, x, y) {
  
  u = v = w = 0 
  
  for (k in 1:length(str_resampled)) {
    
    if (str_resampled[[k]][x, y] == 1) {
      u = u + 1
    } else if (str_resampled[[k]][y, x] == 1) {
      v = v + 1
    } else {
      w = w + 1
    }
    
  } # end for k
  
  df = data.frame(u, v, w)
  colnames(df) = c(paste0(x, "->", y), paste0(x, "<-", y), paste0(x, "..", y))
  return(df)
  
}
##########################################################################################














