libraries = c("bnlearn", "gRain", "gtools", "entropy", "reshape2", "ggplot2")
lapply(libraries, require, character.only = TRUE)
setwd("Experiments")
# setwd("/Users/Administrator/Desktop/Experiments") home pc wd
options(scipen = 10) # print numerical values to fixed within 10 digits instead of exponential
options(digits = 3) # print to the 3rd decimal place, default is 7

################################### generate BN and data ##################################
generateSeed = function() {
  op = options(digits.secs = 6)
  x = gsub("[: -]", "" , Sys.time(), perl = TRUE) # remove - and : 
  x = strsplit(x, split = "[.]")[[1]][2] # get lower digits
  x = as.numeric(x) # convert char to numeric 
  return(x)
}

# it takes about 1/2 min to save a 150kb dsc file
# hence save cpts to .net (Hugin)
generateBNAndData = function(numNodes, maxNumParents, maxNumValues, associationStrength, 
                             numInstances, numIterations, currentDirectory, debug = FALSE) {
  if (debug) cat("* generating BN with numNodes", numNodes, "maxNumParents", maxNumParents, "maxNumValues", maxNumValues, 
                 "associationStrength", associationStrength, "numInstances", numInstances,  "\n")
  seed = generateSeed()
  set.seed(seed)
  dag = generateDag(numNodes, maxNumParents) # generate dag
  cpts = generateCPTs(dag, maxNumValues, associationStrength) # sample cpts from dirichlet distribution
  dataTraining = rbn(cpts, numInstances*numIterations) # generate training data
  dataTesting = rbn(cpts, 2000) # generate testing data
  # construct file name
  partialFileName = paste(numNodes, maxNumParents, maxNumValues, 
                          associationStrength, numInstances, sep = "_")
  
  if (debug) cat("* saving BN \n")
  write.dot(paste0(currentDirectory, "/True networks/Structures/", 
                   partialFileName, "_", seed, ".dot"), dag)
  write.net(paste0(currentDirectory, "/True networks/CPTs/", 
                   partialFileName, "_", seed, ".net"), cpts)
  if (debug) cat("* saving data \n")
  write.csv(dataTraining, paste0(currentDirectory, "/Datasets/", 
                         partialFileName, "_", seed, "_training.csv"), row.names = FALSE)
  write.csv(dataTesting, paste0(currentDirectory, "/Datasets/", 
                         partialFileName, "_", seed, "_testing.csv"), row.names = FALSE)
}

# re-order files as the same order appear in folder
orderFiles = function(files, currentDirectory, numIterations) {
  numbersInName = matrix(0, nrow = length(files), ncol = 6) # empty matrix for storing values in file name
  for (i in 1:length(files)) {
    numbersInName[i,] = na.omit(as.numeric(unlist(strsplit(files[i], "[^0-9]+")))) # extract valuesi n file name
  }
  colnames(numbersInName) = c("numNodes", "maxNumParents", "maxNumValues", 
                              "associationStrength", "numInstances", "seed") # assign column names to matrix
  numbersInName = numbersInName[,c(currentDirectory, "seed")] # extract the unfixed column and seed 
  firstOrder = order(numbersInName[,currentDirectory]) # order unfixed column by increasing
  numbersInName = numbersInName[firstOrder,] 
  orderedFiles = files[firstOrder] # re-order files
  
  for (j in 1:(length(files)/numIterations)) { # only order column seed when there are more 1 file for the same instantiation
    if (j > 1) {
      secondOrder = order(numbersInName[((j-1)*numIterations+1):(j*numIterations), 2])
      orderedFiles[((j-1)*numIterations+1):(j*numIterations)] = orderedFiles[((j-1)*numIterations+1):(j*numIterations)][secondOrder]
    }
  }
  return(orderedFiles)
}


################################# Structure learning ####################################
# current learning algorithms including 
# bnlearn::mmhc, aic, bic, bde, k2
# other algorithms that will be included in are 
# pcalg::pc, ges
#########################################################################################
autoLearn = function(currentDirectory, learningMethod, numIterations, debug = FALSE) {
  allTrainingData = list.files(paste0(currentDirectory, "/Datasets"), pattern = "_training.csv") # list all training data in folder
  allTrainingData = orderFiles(allTrainingData, currentDirectory, numIterations) # re-order all files
  for (i in 1:length(allTrainingData)) {
    dataPool = read.csv(paste0(currentDirectory, "/Datasets/", allTrainingData[i]), header = TRUE)
    j = 1
    while (j <= numIterations) { # repeat the process for numIterations times
      seed = generateSeed() 
      set.seed(seed)
      index = sample(1:nrow(dataPool), nrow(dataPool)/numIterations) # sample rows from dataPool for individual traning data
      data = dataPool[index,] # extract rows from dataPool 
      # construct file name
      fileName = paste0(c(na.omit(as.numeric(unlist(strsplit(allTrainingData[i], "[^0-9]+"))))[1:5], seed), collapse = "_")
      # learn structures from data
      if (debug) cat("* learning the ", c(i, j), "structure \n")
      if (learningMethod == "mmhc") { # mmhc algorithm
        dagLearned = mmhc(data)
      } else { # K2, BDe, AIC, BIC all use hill-climbing for seaching
        dagLearned = hc(x = data, score = learningMethod)
      } 
      # estimate cpts using bn.fit with method = bayes to avoid 0 in cpts values
      cptsLearned = bn.fit(dagLearned, data, method = "bayes") 
      # save structure to .dot, cpts to .net files
      if (debug) cat("* saving the ", c(i, j), "structure \n")
      write.dot(paste0(currentDirectory, "/Learned networks/Structures/", learningMethod, "/", fileName, ".dot"), dagLearned)
      write.net(paste0(currentDirectory, "/Learned networks/CPTs/", learningMethod, "/", fileName, ".net"), cptsLearned)
      
      j = j + 1
    }
  }  
}

################################# Evaluations ###########################################
# evaluation measures including 
# accuracy: TPR, FPR
# precision, recall
# f measure
# shd/edit distance for: dags, cpdags, skeletons
# rmse with targetNode randomly selected
# bir with targetNode randomly selected

# count the tp, fp and fn 
# use bnlearn::compare
autoAccuracy = function(currentDirectory, learningMethod, debug = FALSE) {
  allTrueCPTs = list.files(paste0(currentDirectory, "/True networks/CPTs"))
  allTrueCPTs = orderFiles(allTrueCPTs, currentDirectory, 1)
  allLearnedCPTs = list.files(paste0(currentDirectory, "/Learned networks/CPTs/", learningMethod))
  numIterations = length(allLearnedCPTs)/length(allTrueCPTs)
  allLearnedCPTs = orderFiles(allLearnedCPTs, currentDirectory, numIterations)
  
  numTP = rep(0, length(allLearnedCPTs))
  numFP = rep(0, length(allLearnedCPTs))
  numFN = rep(0, length(allLearnedCPTs))
  
  for (i in 1:length(allTrueCPTs)) {
    cptsTrue = read.net(paste0(currentDirectory, "/True networks/CPTs/", allTrueCPTs[i])) # read cpts
    dagTrue = model2network(modelstring(cptsTrue)) # convert cpts to dag
    for (j in ((i-1)*numIterations + 1):(i*numIterations)) {
      if (debug) cat("* calculating accuracy for the", j, "learned dag \n")
      cptsLearned = read.net(paste0(currentDirectory, "/Learned networks/CPTs/", learningMethod, "/", allLearnedCPTs[j])) # read cpts
      dagLearned = model2network(modelstring(cptsLearned)) # convert cpts to dag
      accuracy = bnlearn::compare(dagTrue, dagLearned) # compare the learned dag with the true dag
      
      numTP[j] = accuracy$tp # true positive
      numFP[j] = accuracy$fp # false positive
      numFN[j] = accuracy$fn # false negative
    }
  }
  # save counting results to directory
  write.csv(numTP, paste0(currentDirectory, "/Evaluations/accuracy/", learningMethod, "_TP.csv"), row.names = FALSE)
  write.csv(numFP, paste0(currentDirectory, "/Evaluations/accuracy/", learningMethod, "_FP.csv"), row.names = FALSE)
  write.csv(numFN, paste0(currentDirectory, "/Evaluations/accuracy/", learningMethod, "_FN.csv"), row.names = FALSE)
}

# compute precision and recall from accuracy above
autoPrecisionRecall = function(currentDirectory, numIterations) {
  allTPs = list.files(paste0(currentDirectory, "/Evaluations/accuracy"), pattern = "_TP.csv")
  allFPs = list.files(paste0(currentDirectory, "/Evaluations/accuracy"), pattern = "_FP.csv")
  allFNs = list.files(paste0(currentDirectory, "/Evaluations/accuracy"), pattern = "_FN.csv")
  for (i in 1:length(allTPs)) {
    TP = read.csv(paste0(currentDirectory, "/Evaluations/accuracy/", allTPs[i]), header = TRUE)
    FP = read.csv(paste0(currentDirectory, "/Evaluations/accuracy/", allFPs[i]), header = TRUE)
    FN = read.csv(paste0(currentDirectory, "/Evaluations/accuracy/", allFNs[i]), header = TRUE)
    precision = TP/(TP + FP)
    precision[is.na(precision)] = 0 # replace NA by 0
    recall = TP/(TP + FN)
    recall[is.na(recall)] = 0 # replace NA by 0
    learningMethod = strsplit(allTPs[i], split = "\\_")[[1]][1]
    
    write.csv(precision, paste0(currentDirectory, "/Evaluations/precision/", learningMethod, ".csv"), row.names = FALSE)
    write.csv(recall, paste0(currentDirectory, "/Evaluations/recall/", learningMethod, ".csv"), row.names = FALSE)
    
    autoStats(currentDirectory, learningMethod, "precision", as.vector(t(precision)), numIterations, alpha = 0.05)
    
    autoStats(currentDirectory, learningMethod, "recall", as.vector(t(recall)), numIterations, alpha = 0.05)
  }
}

# compute f measure from accuracy above
autoFMeasure = function(currentDirectory, numIterations) {
  allTPs = list.files(paste0(currentDirectory, "/Evaluations/accuracy"), pattern = "_TP.csv")
  allFPs = list.files(paste0(currentDirectory, "/Evaluations/accuracy"), pattern = "_FP.csv")
  allFNs = list.files(paste0(currentDirectory, "/Evaluations/accuracy"), pattern = "_FN.csv")
  for (i in 1:length(allTPs)) {
    TP = read.csv(paste0(currentDirectory, "/Evaluations/accuracy/", allTPs[i]), header = TRUE)
    FP = read.csv(paste0(currentDirectory, "/Evaluations/accuracy/", allFPs[i]), header = TRUE)
    FN = read.csv(paste0(currentDirectory, "/Evaluations/accuracy/", allFNs[i]), header = TRUE)
    precision = TP/(TP + FP)
    recall = TP/(TP + FN)
    fMeasure = 2*(precision * recall)/(precision + recall)
    fMeasure[is.na(fMeasure)] = 0 # replace NA with 0 
    learningMethod = strsplit(allTPs[i], split = "\\_")[[1]][1] # get learning method from file name
    write.csv(fMeasure, paste0(currentDirectory, "/Evaluations/f measure/", learningMethod, ".csv"), row.names = FALSE)
    
    autoStats(currentDirectory, learningMethod, "f measure", as.vector(t(fMeasure)), numIterations, alpha = 0.05)
  }
}

# compute shd/edit distance for cpdags dags, skeletons
# bnlearn::shd for cpdags
# bnlearn::hamming for skeletons
# hammingDags for dags (own function)
autoSHD = function(currentDirectory, learningMethod, debug = FALSE) {
  allTrueCPTs = list.files(paste0(currentDirectory, "/True networks/CPTs"))
  allTrueCPTs = orderFiles(allTrueCPTs, currentDirectory, 1)
  allLearnedCPTs = list.files(paste0(currentDirectory, "/Learned networks/CPTs/", learningMethod))
  numIterations = length(allLearnedCPTs)/length(allTrueCPTs)
  allLearnedCPTs = orderFiles(allLearnedCPTs, currentDirectory, numIterations)
  # 3 empty score vectors
  shdCpdags = rep(0, length(allLearnedCPTs)) 
  shdSkeletons = shdCpdags
  shdDags = shdCpdags
  
  for (i in 1:length(allTrueCPTs)) {
    cptsTrue = read.net(paste0(currentDirectory, "/True networks/CPTs/", allTrueCPTs[i]))
    dagTrue = model2network(modelstring(cptsTrue))
    for (j in ((i-1)*numIterations + 1):(i*numIterations)) {
      if (debug) {
        cat("* calculating SHD for the ", j, "file \n")
      }
      cptsLearned = read.net(paste0(currentDirectory, "/Learned networks/CPTs/", learningMethod, "/", allLearnedCPTs[j]))
      dagLearned = model2network(modelstring(cptsLearned))
      # shd for cpdags
      shdCpdags[j] = bnlearn::shd(learned = dagLearned, true = dagTrue)
      # shd for skeletons
      shdSkeletons[j] = hamming(learned = dagLearned, true = dagTrue)
      # shd for dags
      shdDags[j] = hammingDags(learned = dagLearned, true = dagTrue)
    }
  }
  
  write.csv(shdCpdags, paste0(currentDirectory, "/Evaluations/shd cpdags/", learningMethod, ".csv"), row.names = FALSE)
  write.csv(shdDags, paste0(currentDirectory, "/Evaluations/shd dags/", learningMethod, ".csv"), row.names = FALSE)
  write.csv(shdSkeletons, paste0(currentDirectory, "/Evaluations/shd skeletons/", learningMethod, ".csv"), row.names = FALSE)
  
  autoStats(currentDirectory, learningMethod, "shd cpdags", shdCpdags, numIterations, alpha = 0.05)
  
  autoStats(currentDirectory, learningMethod, "shd dags", shdDags, numIterations, alpha = 0.05)
  
  autoStats(currentDirectory, learningMethod, "shd skeletons", shdSkeletons, numIterations, alpha = 0.05)
}

autoRMSEBIR = function(currentDirectory, learningMethod, numIterations, debug = FALSE) {
  testSets = list.files(paste0(currentDirectory, "/Datasets"), pattern = "_testing") # load all test sets
  testSets = orderFiles(testSets, currentDirectory, 1) # order all test sets
  allLearnedCPTs = list.files(paste0(currentDirectory, "/Learned networks/CPTs/", learningMethod)) # load all learned cpts
  allLearnedCPTs = orderFiles(allLearnedCPTs, currentDirectory, numIterations) # order all cpts
  rmseScore = rep(0, length(allLearnedCPTs)) # initial empty vector of rmse
  birScore = rep(0, length(allLearnedCPTs)) # initial empty vector of bir
  for (i in 1:length(testSets)) {
    testingData = read.csv(paste0(currentDirectory, "/Datasets/", testSets[i]), header = TRUE)
    allNodes = colnames(testingData)
    infoReward = rep(0, nrow(testingData)) # initial empty vector of bir for a single learned cpts
    squareError = infoReward # initial empty vector of squareError for a single learned cpts
    for (j in ((i-1)*numIterations + 1):(i*numIterations)) {
      cptsLearned = read.net(paste0(currentDirectory, "/Learned networks/CPTs/", learningMethod, "/", allLearnedCPTs[j]))
      # estimate rmse and bir by compute score for each row for a sampled target node
      for (k in 1:nrow(testingData)) {
        if (debug) cat("** calculating score for file", j, "row", k, "in test set \n")
        targetNode = sample(allNodes, 1) # sample a new target
        prior = freqs(table(testingData[,targetNode])) # prior density for the newly sampled target 
        res = rmseBIR3(testingData[k,], cptsLearned, targetNode, prior)
        infoReward[k] = res$infoReward
        squareError[k] = res$squareError
      }
      birScore[j] = mean(infoReward) # mean of bir for a single learned cpts
      rmseScore[j] = sqrt(mean(squareError)) # mean of rmse for a single learned cpts
    }
  }
  
  write.csv(rmseScore, paste0(currentDirectory, "/Evaluations/rmse/", learningMethod, ".csv"), row.names = FALSE)
  write.csv(birScore, paste0(currentDirectory, "/Evaluations/bir/", learningMethod, ".csv"), row.names = FALSE)
  
  autoStats(currentDirectory, learningMethod, "rmse", rmseScore, numIterations, alpha = 0.05)
  
  autoStats(currentDirectory, learningMethod, "bir", birScore, numIterations, alpha = 0.05)
}

# compute kld 
# R cannot compute kld for network with 50 nodes due to memory issue
autoKLD = function(currentDirectory, learningMethod, debug = FALSE) {
  allTrueCPTs = list.files(paste0(currentDirectory, "/True networks/CPTs"))
  allTrueCPTs = orderFiles(allTrueCPTs, currentDirectory, 1)
  allLearnedCPTs = list.files(paste0(currentDirectory, "/Learned networks/CPTs/", learningMethod))
  numIterations = length(allLearnedCPTs)/length(allTrueCPTs)
  allLearnedCPTs = orderFiles(allLearnedCPTs, currentDirectory, numIterations)
 
  klDivergence = rep(0, length(allLearnedCPTs))
  
  for (i in 1:length(allTrueCPTs)) {
    cptsTrue = read.net(paste0(currentDirectory, "/True networks/CPTs/", allTrueCPTs[i]))
    for (j in ((i-1)*numIterations + 1):(i*numIterations)) {
      if (debug) {
        cat("* calculating the KLD for the ", j, "file \n")
      }
      cptsLearned = read.net(paste0(currentDirectory, "/Learned networks/CPTs/", learningMethod, "/", allLearnedCPTs[j]))
      klDivergence[j] = kld(cptsTrue, cptsLearned, debug = debug)[[1]] # compute kld between true and learned
    }
  }
  
  write.csv(klDivergence, paste0(currentDirectory, "/Evaluations/kld/", learningMethod, ".csv"), row.names = FALSE)
  
  autoStats(currentDirectory, learningMethod, "kld", klDivergence, numIterations)
  
}

autoStats = function(currentDirectory, learningMethod, measure, vector, numIterations, alpha = 0.05) {
  average = rep(0, length(vector)/numIterations) # empty mean
  std = average # empty sd
  for (i in 1:length(average)) { 
    average[i] = mean(vector[((i-1)*numIterations+1):(i*numIterations)]) # compute mean 
    std[i] = sd(vector[((i-1)*numIterations+1):(i*numIterations)]) # compute sd
  }
  error = qnorm(1 - alpha/2)*std/sqrt(numIterations) # compute error for confidence interval
  stats = data.frame(average, std, error) # store statistics into data frame
  # save into direcotry 
  write.csv(stats, paste0(currentDirectory, "/Evaluations/", measure, "/stats/stats_", learningMethod, ".csv"), row.names = FALSE)
}

######################## Execution starts from here ############################
numNodes = 50
maxNumParents = 4
maxNumValues = 3
associationStrength = 5
numInstances = 20000


# unfixed: numInstances
numInstancesList = c(seq(100, 900, 100), seq(1000, 9000, 1000), seq(10000, 100000, 10000))
sapply(numInstancesList, generateBNAndData, numNodes = numNodes, maxNumParents = maxNumParents, maxNumValues = maxNumValues, 
       associationStrength = associationStrength, numIterations = 20, currentDirectory = "numInstances", debug = TRUE)


# unfixed: numNodes
# Kel PC only able to generate up to 200 nodes
numNodesList = c(3, 13, 23, 33, 43, 53, 63, 73, 83, 93, 100)
numNodesList = c(200, 300, 400, 500, 600, 700, 800, 900, 1000)
sapply(numNodesList, generateBNAndData, maxNumParents = maxNumParents, maxNumValues = maxNumValues, 
       associationStrength = associationStrength, numInstances = numInstances, numIterations = 20, currentDirectory = "numNodes", debug = TRUE)

# unfixed: maxNumParents
maxNumParentsList = 2:11
sapply(maxNumParentsList, generateBNAndData, numNodes = numNodes, maxNumValues = maxNumValues, 
       associationStrength = associationStrength, numInstances = numInstances, numIterations = 20, currentDirectory = "maxNumParents", debug = TRUE)

# unfixed: maxNumValues
maxNumValuesList = 2:10 
sapply(maxNumValuesList, generateBNAndData, numNodes = numNodes, maxNumParents = maxNumParents, 
       associationStrength = associationStrength, numInstances = numInstances, numIterations = 20, currentDirectory = "maxNumValues", debug = TRUE)

# unfixed: associationStrength
#strengthList = c(1, 50, 100, 150, 200)
strengthList = c(1, 10, 20, 30, 40, 50, 75, 100, 125, 150, 175, 200, 300, 400, 500)
sapply(strengthList, generateBNAndData, numNodes = numNodes, maxNumParents = maxNumParents, maxNumValues = maxNumValues, 
       numInstances = numInstances, numIterations = 20, currentDirectory = "associationStrength", debug = TRUE)
###################################### structure learning ######################################
sapply(c("mmhc", "aic", "bic", "bde", "k2"), autoLearn, currentDirectory = currentDirectory, numIterations = 20, debug = TRUE)

###################################### Evaluations ######################################
# tp fp fn 
sapply(c("mmhc", "aic", "bic", "bde", "k2"), autoAccuracy, currentDirectory = currentDirectory, debug = TRUE)
# precision recall
autoPrecisionRecall(currentDirectory, 20)
# f measure
autoFMeasure(currentDirectory, 20)
# shd
sapply(c("mmhc", "aic", "bic", "bde", "k2"), autoSHD, currentDirectory = currentDirectory, debug = TRUE)

################################################### RMSE ###################################################
sapply(c("mmhc", "aic", "bic", "bde", "k2"), autoRMSEBIR, currentDirectory = currentDirectory, numIterations = 20, debug = TRUE)
sapply(c("bde", "k2"), autoRMSEBIR, currentDirectory = currentDirectory, numIterations = 20, debug = TRUE)

sapply(c("aic", "bic", "bde", "k2"), autoKLD, currentDirectory = currentDirectory, debug = TRUE)
##################################### save plots #######################################
numIterations = 20

measure = "precision"
measure = "recall"
measure = "f measure"
measure = "shd cpdags"
measure = "shd dags"
measure = "shd skeletons"
measure = "rmse"
measure = "bir"
measure = "kld"

# unfixed parameter
currentDirectory = "maxNumParents"

if (currentDirectory == "maxNumParents") {
  ls = maxNumParentsList
} else if (currentDirectory == "maxNumValues") {
  ls = maxNumValuesList
} else if (currentDirectory == "associationStrength") {
  ls = strengthList
} else if (currentDirectory == "numInstances") {
  ls = numInstancesList
} else if (currentDirectory == "numNodes") {
  ls = numNodesList
}

df_k2 = read.csv(paste0(currentDirectory, "/Evaluations/", measure, "/stats/stats_k2.csv"), header = TRUE)
df_mmhc = read.csv(paste0(currentDirectory, "/Evaluations/", measure, "/stats/stats_mmhc.csv"), header = TRUE)
df_aic = read.csv(paste0(currentDirectory, "/Evaluations/", measure, "/stats/stats_aic.csv"), header = TRUE)
df_bic = read.csv(paste0(currentDirectory, "/Evaluations/", measure, "/stats/stats_bic.csv"), header = TRUE)
df_bde = read.csv(paste0(currentDirectory, "/Evaluations/", measure, "/stats/stats_bde.csv"), header = TRUE)
tempMean = cbind(ls, df_k2$average, df_mmhc$average, df_aic$average, df_bic$average, df_bde$average)
colnames(tempMean) = c(currentDirectory, "k2", "mmhc", "aic", "bic", "bde")

tempMean = as.data.frame(tempMean) # convert matrix to data.frame
meltTempMean = melt(tempMean, id = currentDirectory)
error = c(df_k2$error, df_mmhc$error, df_aic$error, df_bic$error, df_bde$error) # standard error

figure = ggplot(meltTempMean, aes(x = meltTempMean[,1], y = value, color = variable)) + 
  geom_line(data = meltTempMean) +ylab(label = measure) + xlab(currentDirectory)
figure = figure + geom_errorbar(aes(ymin = value - error, ymax = value + error), width = 0.2) + 
  ylim(min(meltTempMean$value-error), max(meltTempMean$value+error))
figure
figure = figure + ylim(0.6,0.95)
ggsave(filename = paste0(currentDirectory, "/Plots/", measure, "_", currentDirectory, ".png"))


# plot with zoomed region
figure + coord_cartesian(xlim = c(0,200), ylim = c(0.68,0.71))





