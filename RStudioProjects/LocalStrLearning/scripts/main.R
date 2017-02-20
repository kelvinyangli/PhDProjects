# In this script, we use full search for MBPTs and compare against others
#
# Test 1: compare with polytree search algorithms. The objective is to demonstarte that
# by searching through all possible MBPTs, we are able to find the MBPT that is
# a sub-graph of the true local structure within the MB. Since the returned str is a
# polytree, we may want to compare our method with polytree learning algorithm without
# considering true and false negatives.


adjmtx = randPolytree(12, 3)
pt = matrix2dag(adjmtx)
graphviz.plot(pt)
cpts = randCPTs(pt, 2, 1)
n = 1000
data = rbn(cpts, n)
dataInfo = getDataInfo(data)
vars = colnames(adjmtx)
for (i in 1:length(vars)) { # iterate through all vars 
  
  target = vars[i]
  cat(target, ": \n")
  mbVars = mBlkt(adjmtx, target) # extract mb(x) from the true model
  mbpts = readRDS(paste0("MBPTs/", length(mbVars), ".rds")) # load pre-saved mbpts for mb size n
  mbpts = substituteVar(mbpts, target, mbVars) # replace default vars with mb vars
  mmlmtx = computeMMLMatrix(vars, mbVars, target, dataInfo, n) # compute mmlcpt for each node in mbVars given its possible parents
  # compute mmlcpt for each mbpt 
  scores = rep(0, length(mbpts))
  for (j in 1:length(mbpts)) scores[j] = mmlDag_fast(mbpts[[j]], vars, dataInfo, mmlmtx, n)
  index = which.min(scores) # find the minimum score's index
  mbptLearned = mbpts[index][[1]][c(target, mbVars),c(target,mbVars)] # the learned mbpt
  mbTrue = adjmtx[c(target, mbVars),c(target,mbVars)] # extract local str within mb(x) and compare with the learned mbpt
  print(mbptLearned)
  cat("---------------- \n")
  # compare the learnd mbpt with the true mb
  # at the moment we don't care about false negatives, because mbpt is a sub-graph of mb's local str
  # we only care about false positives, i.e., those arcs that are learned but not in the true local str
  (acc = strAccuracy(mbTrue, mbptLearned))
  (precision = (sum(mbptLearned) - acc$delete - acc$reverse) / sum(mbptLearned))
  (recall = (sum(mbptLearned) - acc$delete - acc$reverse) / sum(mbTrue))
  
}

# visulize learned mbpt and the true mb
par(mfrow = c(2, 2))
graphviz.plot(matrix2dag(mbLocalStr(adjmtx, vars, vars[i], mbVars)), main = "true")
graphviz.plot(matrix2dag(mbptLearned), main = "learned")
# once the optimal mbpt is learned, we use this result as the start of the next step
# which is completing the remaining arcs.
# this can be achieved in various ways, but the most straightfoward method is to 
# use greedy search, i.e. iteratively adding different edges and compare the mml
# socre for the entire structure, if the score is decreased, then the edge is 
# accepted, otherwise just keep going.
# by the end of this process, we hope that the resulting str is as close to the 
# true local str as possible. 





