
numNodes = 37
maxNumParents = 4
maxNumValues = 2
concentration = 1
sampleSize = 10000
numTests = 10

# debug = FALSE

pre.cpt = rep(0, numTests)
re.cpt = rep(0, numTests)

pre.logit = rep(0, numTests)
re.logit = rep(0, numTests)

pre.logit2 = rep(0, numTests)
re.logit2 = rep(0, numTests)

pre.iamb = rep(0, numTests)
re.iamb = rep(0, numTests)

findMB.cpt = findMB.logit = findMB.logit2 = findMB.iamb = 0

par(mfrow = c(1, 2))

for (i in 1:numTests) {

  # generate a random seed for each test 
  seed = generateSeed()
  set.seed(seed)
  
  # generate BNs and data 
  dag = generateDag(numNodes, maxNumParents)
  cpts = generateCPTs(dag, maxNumValues, concentration)
  data = rbn(cpts, sampleSize)

  allNodes = bnlearn::nodes(dag)
  numNodes = length(allNodes)
  
  graphviz.plot(dag)
  
  allNodes = names(data)
  
  datainfo = dataInfo(data)
  
  indicatorMatrix = getIndicator(data)
  
  cat("Test", i, "--- seed", seed, "\n")
  
  ##############################################################################
  # find mb of each node in a dag
  for (j in 1:numNodes) {
    
    mbTrue = bnlearn::mb(cpts, allNodes[j])
    
    mb.cpt = mbForwardSelection(data,allNodes[j], mmlCPT, datainfo$arities, datainfo$indexListPerNodePerValue)
    mb.logit = mbForwardSelection(data, allNodes[j], mmlLogit, datainfo$arities, datainfo$indexListPerNodePerValue, indicatorMatrix = indicatorMatrix)
    mb.logit2 = mbForwardSelection(data, allNodes[j], mmlLogit2ndOrder, datainfo$arities, datainfo$indexListPerNodePerValue, 
                                   indicatorMatrix = indicatorMatrix, interaction = T)
    mb.iamb = learn.mb(data, allNodes[j], method = "iamb")
    
    cat(allNodes[j], "\n")
    
    acc.cpt = mbAccuracy(mbTrue, mb.cpt, allNodes[j], allNodes)
    cat("cpt   :", round(acc.cpt, 3), "\n")
    
    acc.logit = mbAccuracy(mbTrue, mb.logit, allNodes[j], allNodes)
    cat("logit :", round(acc.logit, 3), "\n")
    
    acc.logit2 = mbAccuracy(mbTrue, mb.logit2, allNodes[j], allNodes)
    cat("logit2:", round(acc.logit2, 3), "\n")
    
    acc.iamb = mbAccuracy(mbTrue, mb.iamb, allNodes[j], allNodes)
    cat("iamb  :", round(acc.iamb, 3), "\n")
    cat("-------------------------- \n")
    
    # accumulate correct mb identify
    if (sum(acc.cpt[5:6]) == 2) findMB.cpt = findMB.cpt + 1
    if (sum(acc.logit[5:6]) == 2) findMB.logit = findMB.logit + 1
    if (sum(acc.logit2[5:6]) == 2) findMB.logit2 = findMB.logit2 + 1
    if (sum(acc.iamb[5:6]) == 2) findMB.iamb = findMB.iamb + 1
    
    # accumulate precision and recall for computing the average over all nodes
    pre.cpt[i] = pre.cpt[i] + acc.cpt[5]
    re.cpt[i] = re.cpt[i] + acc.cpt[6]
    
    pre.logit[i] = pre.logit[i] + acc.logit[5]
    re.logit[i] = re.logit[i] + acc.logit[6]
    
    pre.logit2[i] = pre.logit2[i] + acc.logit2[5]
    re.logit2[i] = re.logit2[i] + acc.logit2[6]
    
    pre.iamb[i] = pre.iamb[i] + acc.iamb[5]
    re.iamb[i] = re.iamb[i] + acc.iamb[6]
    
  } # end for j
  
  # report overal precision and recall 
  pre.cpt[i] = pre.cpt[i]/numNodes
  re.cpt[i] = re.cpt[i]/numNodes
  
  pre.logit[i] = pre.logit[i]/numNodes
  re.logit[i] = re.logit[i]/numNodes
  
  pre.logit2[i] = pre.logit2[i]/numNodes
  re.logit2[i] = re.logit2[i]/numNodes
  
  pre.iamb[i] = pre.iamb[i]/numNodes
  re.iamb[i] = re.iamb[i]/numNodes
  
} # end for i

# average f-measure
f.cpt = 2 * pre.cpt * re.cpt / (pre.cpt + re.cpt)
f.logit = 2 * pre.logit * re.logit / (pre.logit + re.logit)
f.logit2 = 2 * pre.logit2 * re.logit2 / (pre.logit2 + re.logit2)
f.iamb = 2 * pre.iamb * re.iamb / (pre.iamb + re.iamb)

# display results
if (debug) {
  
  cat("------------------------------------- \n")
  cat("precision \n")
  cat("cpt   :", round(pre.cpt, 3), "\n")
  cat("logit :", round(pre.logit, 3), "\n")
  cat("logit2:", round(pre.logit2, 3), "\n")
  cat("iamb  :", round(pre.iamb, 3), "\n")
  
  cat("------------------------------------- \n")
  cat("recall \n")
  cat("cpt   :", round(re.cpt, 3), "\n")
  cat("logit :", round(re.logit, 3), "\n")
  cat("logit2:", round(re.logit2, 3), "\n")
  cat("iamb  :", round(re.iamb, 3), "\n")
  
  cat("------------------------------------- \n")
  cat("f-measure \n")
  cat("cpt   :", round(f.cpt, 3), "\n")
  cat("logit :", round(f.logit, 3), "\n")
  cat("logit2:", round(f.logit2, 3), "\n")
  cat("iamb  :", round(f.iamb, 3), "\n")
  
  cat("------------------------------------- \n")
  cat("correct finding out of", numNodes * numTests, "MBs \n")
  cat("cpt   :", findMB.cpt, "\n")
  cat("logit :", findMB.logit, "\n")
  cat("logit2:", findMB.logit2, "\n")
  cat("iamb  :", findMB.iamb, "\n")
}


df = data.frame(pre.cpt, pre.logit, pre.logit2, pre.iamb,
                re.cpt, re.logit, re.logit2, re.iamb,
                f.cpt, f.logit, f.logit2, f.iamb)

meanAcc = apply(df, 2, mean)
sdAcc = apply(df, 2, sd)
seAcc = sdAcc/sqrt(nrow(df))
ci.upper = meanAcc + 1.96 * seAcc
ci.lower = meanAcc - 1.96 * seAcc

par(mfrow = c(1, 2))
x = c("cpt", "logit", "logit2", "iamb")
plotCI(1:4, meanAcc[1:4], ui = ci.upper[1:4], li = ci.lower[1:4], ylab = "precision", main = x)
plotCI(1:4, meanAcc[5:8], ui = ci.upper[5:8], li = ci.lower[5:8], ylab = "recall", main = x)
plotCI(1:4, meanAcc[9:12], ui = ci.upper[9:12], li = ci.lower[9:12], ylab = "f-measure", main = x)

findMB = c(findMB.cpt, findMB.logit, findMB.logit2, findMB.iamb)
plot(1:4, findMB, ylim = c(min(findMB), numNodes * numTests), ylab = "correct MB finds", main = x)





