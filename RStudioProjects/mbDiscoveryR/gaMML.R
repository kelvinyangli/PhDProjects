allNodes = names(data)
nodeIndex = which(allNodes == node) # get index of the target node

numNodes = ncol(data)
sampleSize = nrow(data)

mb = c()
#mb = rep(0, numNodes - 1)
#unCheckedNodes = allNodes[allNodes != node] # remove target node
unCheckedIndecies = (1:numNodes)[-nodeIndex]

##############################################################
# get the arity of each node 
# get the indecies for each value of each node
# 
arities = rep(0, numNodes)

indexListPerNodePerValue = list()

for (i in 1:numNodes) {
  
  arities[i] = nlevels(data[, i])
  
  
  # get the indecides for each value of node i
  indexListPerValue = list()
  
  for (j in 1:arities[i]) {
    
    indexListPerValue[[j]] = which(data[, i] == levels(data[, i])[j]) 
    
  } # end for arity j
  
  indexListPerNodePerValue[[i]] = indexListPerValue
  
} # end for node i

evalFunc = function(x) {
  
  if (sum(x) == 0) {
    len = mmlSingleNode(nodeIndex, indexListPerNodePerValue, arities, sampleSize, 2)
  } else {
    parentsIndecies = unCheckedIndecies[x == 1]
    len = mmlCPT(nodeIndex, parentsIndecies, indexListPerNodePerValue, arities, sampleSize, 2)
  }
  return(len)
}


iters = 20

x = sample(0:1, ncol(data)-1, replace = T)

GAmodel <- rbga.bin(size = (ncol(data)-1),                  # length of chromo
                    popSize = 100,             # population
                    iters = iters,              # max generations
                    mutationChance = 0.1,     # mutation rate
                    elitism = T,               # number of best chromos retained, default is 20%
                    evalFunc = evalFunc, # fitness function
                    verbose = F)       

# GAmodel # works but verbose

summary(GAmodel, echo = T) # ka-ching

# Report the value of the best solution
bestSolution = GAmodel$population[iters,] # best solution is returned result in the last iteration
evalFunc(bestSolution)

# plot of GA results, where the black line is the minimal value and the blue line indicates
# the GA value
plot(GAmodel)