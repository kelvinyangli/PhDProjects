
numNodes = 20
maxNumParents = 3
maxNumValues = 3
concentration = 1
sampleSize = 10000
numTests = 10

debug = TRUE

pre.gs.cpt = rep(0, numTests)
re.gs.cpt = rep(0, numTests)

pre.gs.cpt.revised = rep(0, numTests)
re.gs.cpt.revised = rep(0, numTests)

pre.gs.cpt.lookahead = rep(0, numTests)
re.gs.cpt.lookahead = rep(0, numTests)

#pre.gs.cpt.revised.lookahead = rep(0, numTests)
#re.gs.cpt.revised.lookahead = rep(0, numTests)

#pre.sa.cpt = rep(0, numTests)
#re.sa.cpt = rep(0, numTests)

#pre.sa.cpt.revised = rep(0, numTests)
#re.sa.cpt.revised = rep(0, numTests)

pre.iamb = rep(0, numTests)
re.iamb = rep(0, numTests)

findMB.gs.cpt = findMB.gs.cpt.revised = findMB.gs.cpt.lookahead = findMB.sa.cpt = findMB.iamb = 0

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
  
  cat("Test", i, "--- seed", seed, "\n")
  
  ##############################################################################
  # find mb of each node in a dag
  for (j in 1:numNodes) {
    
    mbTrue = bnlearn::mb(cpts, allNodes[j])
    
    mb.gs.cpt = mbGreedySearch(data, allNodes[j], mmlCPT)  
    mb.gs.cpt.revised = mbGreedySearch(data, allNodes[j], mmlCPT.revised)
    mb.gs.cpt.lookahead = mbGreedySearchWithLookAhead(data, allNodes[j], mmlCPT)
    #mb.gs.cpt.revised.lookahead = mbGreedySearchWithLookAhead(data, allNodes[j], mmlCPT.revised)
    #mb.sa.cpt = mbSimulatedAnnealing(data, allNodes[j], mmlCPT, rep(0, numNodes - 1), 
    #                                 maxIterations = 100, nbStep = 2, step = 0.05)
    #mb.sa.cpt.revised = mbSimulatedAnnealing(data, allNodes[j], mmlCPT.revised, rep(0, numNodes - 1), 
    #                                 maxIterations = 100, nbStep = 2, step = 0.05)
    mb.iamb = learn.mb(data, allNodes[j], method = "iamb")
    
    cat(allNodes[j], "\n")
    
    acc.gs.cpt = mbAccuracy(mbTrue, mb.gs.cpt, allNodes[j], allNodes)
    cat("gs+cpt:                  ", round(acc.gs.cpt, 3), "\n")
    
    acc.gs.cpt.revised = mbAccuracy(mbTrue, mb.gs.cpt.revised, allNodes[j], allNodes)
    cat("gs+cpt.revised:          ", round(acc.gs.cpt.revised, 3), "\n")
    
    acc.gs.cpt.lookahead = mbAccuracy(mbTrue, mb.gs.cpt.lookahead, allNodes[j], allNodes)
    cat("gs+cpt+lookahead:        ", round(acc.gs.cpt.lookahead, 3), "\n")
    
    #acc.sa.cpt = mbAccuracy(mbTrue, mb.sa.cpt, allNodes[j], allNodes)
    #cat("sa+cpt:                  ", round(acc.sa.cpt, 3), "\n")
    
    acc.iamb = mbAccuracy(mbTrue, mb.iamb, allNodes[j], allNodes)
    cat("iamb:                    ", round(acc.iamb, 3), "\n")
    cat("-------------------------- \n")
    
    # accumulate correct mb identify
    if (sum(acc.gs.cpt[5:6]) == 2) findMB.gs.cpt = findMB.gs.cpt + 1
    if (sum(acc.gs.cpt.revised[5:6]) == 2) findMB.gs.cpt.revised = findMB.gs.cpt.revised + 1
    if (sum(acc.gs.cpt.lookahead[5:6]) == 2) findMB.gs.cpt.lookahead = findMB.gs.cpt.lookahead + 1
    #if (sum(acc.sa.cpt[5:6]) == 2) findMB.sa.cpt = findMB.sa.cpt + 1
    if (sum(acc.iamb[5:6]) == 2) findMB.iamb = findMB.iamb + 1
    
    # accumulate precision and recall for computing the average over all nodes
    pre.gs.cpt[i] = pre.gs.cpt[i] + acc.gs.cpt[5]
    re.gs.cpt[i] = re.gs.cpt[i] + acc.gs.cpt[6]
    
    pre.gs.cpt.revised[i] = pre.gs.cpt.revised[i] + acc.gs.cpt.revised[5]
    re.gs.cpt.revised[i] = re.gs.cpt.revised[i] + acc.gs.cpt.revised[6]
    
    pre.gs.cpt.lookahead[i] = pre.gs.cpt.lookahead[i] + acc.gs.cpt.lookahead[5]
    re.gs.cpt.lookahead[i] = re.gs.cpt.lookahead[i] + acc.gs.cpt.lookahead[6]
    
    #pre.gs.cpt.revised.lookahead[i] = pre.gs.cpt.revised.lookahead[i] + acc.gs.cpt.revised.lookahead[5]
    #re.gs.cpt.revised.lookahead[i] = re.gs.cpt.revised.lookahead[i] + acc.gs.cpt.revised.lookahead[6]
    
    #pre.sa.cpt[i] = pre.sa.cpt[i] + acc.sa.cpt[5]
    #re.sa.cpt[i] = re.sa.cpt[i] + acc.sa.cpt[6]
    
    #pre.sa.cpt.revised[i] = pre.sa.cpt.revised[i] + acc.sa.cpt.revised[5]
    #re.sa.cpt.revised[i] = re.sa.cpt.revised[i] + acc.sa.cpt.revised[6]
    
    pre.iamb[i] = pre.iamb[i] + acc.iamb[5]
    re.iamb[i] = re.iamb[i] + acc.iamb[6]
    
  } # end for j
  
  # report overal precision and recall 
  pre.gs.cpt[i] = pre.gs.cpt[i]/numNodes
  re.gs.cpt[i] = re.gs.cpt[i]/numNodes
  
  pre.gs.cpt.revised[i] = pre.gs.cpt.revised[i]/numNodes
  re.gs.cpt.revised[i] = re.gs.cpt.revised[i]/numNodes
  
  pre.gs.cpt.lookahead[i] = pre.gs.cpt.lookahead[i]/numNodes
  re.gs.cpt.lookahead[i] = re.gs.cpt.lookahead[i]/numNodes
  
  #pre.gs.cpt.revised.lookahead[i] = pre.gs.cpt.revised.lookahead[i]/numNodes
  #re.gs.cpt.revised.lookahead[i] = re.gs.cpt.revised.lookahead[i]/numNodes
  
  #pre.sa.cpt[i] = pre.sa.cpt[i]/numNodes
  #re.sa.cpt[i] = re.sa.cpt[i]/numNodes
  
  #pre.sa.cpt.revised[i] = pre.sa.cpt.revised[i]/numNodes
  #re.sa.cpt.revised[i] = re.sa.cpt.revised[i]/numNodes
  
  pre.iamb[i] = pre.iamb[i]/numNodes
  re.iamb[i] = re.iamb[i]/numNodes
  
} # end for i

# average f-measure
f.gs.cpt = 2 * pre.gs.cpt * re.gs.cpt / (pre.gs.cpt + re.gs.cpt)
f.gs.cpt.revised = 2 * pre.gs.cpt.revised * re.gs.cpt.revised / (pre.gs.cpt.revised + re.gs.cpt.revised)
f.gs.cpt.lookahead = 2 * pre.gs.cpt.lookahead * re.gs.cpt.lookahead / (pre.gs.cpt.lookahead + re.gs.cpt.lookahead)
#f.gs.cpt.revised.lookahead = 2 * pre.gs.cpt.revised.lookahead * re.gs.cpt.revised.lookahead / (pre.gs.cpt.revised.lookahead + re.gs.cpt.revised.lookahead)
#f.sa.cpt = 2 * pre.sa.cpt * re.sa.cpt / (pre.sa.cpt + re.sa.cpt)
#f.sa.cpt.revised = 2 * pre.sa.cpt.revised * re.sa.cpt.revised / (pre.sa.cpt.revised + re.sa.cpt.revised)
f.iamb = 2 * pre.iamb * re.iamb / (pre.iamb + re.iamb)

# display results
if (debug) {
  
  cat("------------------------------------- \n")
  cat("precision \n")
  cat("gs+cpt:          ", round(pre.gs.cpt, 3), "\n")
  cat("gs+cpt.revised:  ", round(pre.gs.cpt.revised, 3), "\n")
  cat("gs+cpt+lookahead:", round(pre.gs.cpt.lookahead, 3), "\n")
  #cat("sa+cpt:          ", round(pre.sa.cpt, 3), "\n")
  #cat("sa+cpt.revised:", round(pre.sa.cpt.revised, 3), "\n")
  cat("iamb:            ", round(pre.iamb, 3), "\n")
  
  cat("------------------------------------- \n")
  cat("recall \n")
  cat("gs+cpt:          ", round(re.gs.cpt, 3), "\n")
  cat("gs+cpt.revised:  ", round(re.gs.cpt.revised, 3), "\n")
  cat("gs+cpt+lookahead:", round(re.gs.cpt.lookahead, 3), "\n")
  #cat("sa+cpt:          ", round(re.sa.cpt, 3), "\n")
  #cat("sa+cpt.revised:", round(re.sa.cpt.revised, 3), "\n")
  cat("iamb:            ", round(re.iamb, 3), "\n")
  
  cat("------------------------------------- \n")
  cat("f-measure \n")
  cat("gs+cpt:          ", round(f.gs.cpt, 3), "\n")
  cat("gs+cpt.revised:  ", round(f.gs.cpt.revised, 3), "\n")
  cat("gs+cpt+lookahead:", round(f.gs.cpt.lookahead, 3), "\n")
  #cat("sa+cpt:          ", round(f.sa.cpt, 3), "\n")
  #cat("sa+cpt.revised:", round(f.sa.cpt.revised, 3), "\n")
  cat("iamb:            ", round(f.iamb, 3), "\n")
  
  cat("------------------------------------- \n")
  cat("correct finding out of", numNodes * numTests, "MBs \n")
  cat("gs+cpt:          ", findMB.gs.cpt, "\n")
  cat("gs+cpt.revised:  ", findMB.gs.cpt.revised, "\n")
  cat("gs+cpt+lookahead:", findMB.gs.cpt.lookahead, "\n")
  #cat("sa+cpt:          ", findMB.sa.cpt, "\n")
  cat("iamb:            ", findMB.iamb, "\n")
}


df = data.frame(pre.gs.cpt, pre.gs.cpt.revised, pre.gs.cpt.lookahead,pre.iamb,
                re.gs.cpt, re.gs.cpt.revised, re.gs.cpt.lookahead, re.iamb,
                f.gs.cpt, f.gs.cpt.revised, f.gs.cpt.lookahead, f.iamb)

meanAcc = apply(df, 2, mean)
sdAcc = apply(df, 2, sd)
seAcc = sdAcc/sqrt(nrow(df))
ci.upper = meanAcc + 1.96 * seAcc
ci.lower = meanAcc - 1.96 * seAcc

par(mfrow = c(2, 2))
x = c("gs+cpt", "gs+cpt.revised", "gs+lookahead+cpt", "iamb")
plotCI(1:4, meanAcc[1:4], ui = ci.upper[1:4], li = ci.lower[1:4], ylab = "precision", main = x)
plotCI(1:4, meanAcc[5:8], ui = ci.upper[5:8], li = ci.lower[5:8], ylab = "recall", main = x)
plotCI(1:4, meanAcc[9:12], ui = ci.upper[9:12], li = ci.lower[9:12], ylab = "f-measure", main = x)

findMB = c(findMB.gs.cpt, findMB.gs.cpt.revised, findMB.gs.cpt.lookahead, findMB.iamb)
plot(1:4, findMB, ylim = c(min(findMB), numNodes * numTests), ylab = "correct MB finds", main = x)





