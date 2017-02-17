# In this script, we use full search for MBPTs and compare against others
#
# Test 1: compare with polytree search algorithms. The objective is to demonstarte that
# by searching through all possible MBPTs, we are able to find the MBPT that is
# a sub-graph of the true local structure within the MB. Since the returned str is a
# polytree, we may want to compare our method with polytree learning algorithm without
# considering true and false negatives.

adjmtx = randAdjmtx(5, 2)
dag = matrix2dag(adjmtx)
graphviz.plot(dag)
cpts = randCPTs(dag, 2, 1)
n = 1000
data = rbn(cpts, n)
vars = colnames(adjmtx) # all vars
for (i in 1:length(vars)) { # iterate through all vars 
  
  target = vars[i]
  
  mbVars = mBlkt(adjmtx, target) # extract mb(x) from the true model
  mbpts = readRDS(paste0("MBPTs/", length(mbVars), ".rds")) # load pre-saved mbpts for mb size n
  mbpts = substituteVar(mbpts, target, mbVars) # replace default vars with mb vars
  dataInfo = getDataInfo(data)
  mmlmtx = computeMMLMatrix(vars, mbVars, target, dataInfo, n) # compute mmlcpt for each node in mbVars given its possible parents
  
  # compute mmlcpt for each mbpt 
  scores = rep(0, length(mbpts))
  for (i in 1:length(mbpts)) scores[i] = mmlDag_fast(mbpts[[i]], vars, dataInfo, mmlmtx, n)
  index = which.min(scores) # find the minimum score's index
  mbptLearned = mbpts[index][[1]][c(target, mbVars),c(target,mbVars)] # the learned mbpt
  mbTrue = adjmtx[c(target, mbVars),c(target,mbVars)] # extract local str within mb(x) and compare with the learned mbpt
  
  # compare the learnd mbpt with the true mb
  # at the moment we don't care about false negatives, because mbpt is a sub-graph of mb's local str
  # we only care about false positives, i.e., those arcs that are learned but not in the true local str
  acc = strAccuracy(mbTrue, mbptLearned)
  
}









