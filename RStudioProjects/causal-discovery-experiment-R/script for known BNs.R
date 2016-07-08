######################################################################################################################
# target node, Asia: either, Sachs: PKA, Insurance: ThisCarCost, Alarm: VENTALV 

############################################## load libraries ########################################################
setwd("Experiments/known BNs")
library(bnlearn)
library(entropy)
library(gtools)
library(gRain) 
# to install package RBGL for gRain, copy the next two lines
# source("https://bioconductor.org/biocLite.R")
# biocLite("RBGL")
library(ggplot2)
library(reshape2)

options(scipen = 10)


########################################################### data generation ###################################
x = 1:9
x = x[sort(rep(x, 20))]
numInstances = c(x*100, x*1000, x*10000, rep(100000, 20))

currentDirectory = "insurance"

cpts = read.dsc(paste0(currentDirectory, "/", currentDirectory, ".dsc"))

for (j in 1:length(numInstances)) {
  
  seed = generateSeed()
  set.seed(seed)
  
  dataTraining = rbn(cpts, numInstances[j]) # generate training data
  dataTesting = rbn(cpts, 10000) # generate 10000 testing data
  
  # convert testing data to numeric fomat using toNumeric and save into Datasets/Numeric
  dataTestingNumeric = toNumeric(dataTesting)
  
  # convert training data to numeric for the use of matlab
  dataTrainingNumeric = toNumeric(dataTraining)
  
  fileName = paste(currentDirectory, numInstances[j], seed, sep = "_")
  
  saveRDS(dataTraining, paste0(currentDirectory, "/Datasets/Training/", fileName, ".rds"))
  
  saveRDS(dataTesting, paste0(currentDirectory, "/Datasets/Testing/", fileName, ".rds"))
  
  saveRDS(dataTestingNumeric, paste0(currentDirectory, "/Datasets/Numeric/", fileName, ".rds"))
  
  write.csv(dataTrainingNumeric, paste0(currentDirectory, "/Datasets/Training numeric/", fileName, ".csv"), row.names = FALSE)
  
}


################################################### structure learning ###################################
sapply(c("aic", "bic", "bde", "k2", "mmhc"), autoLearnWithMWSTKnownBN, currentDirectory = "asia", numIterations = 20)
sapply(c("aic", "bic", "bde", "k2", "mmhc"), autoLearnWithMWSTKnownBN, currentDirectory = "sachs", numIterations = 20)
sapply(c("aic", "bic", "bde", "k2", "mmhc"), autoLearnWithMWSTKnownBN, currentDirectory = "insurance", numIterations = 20)
sapply(c("aic", "bic", "bde", "k2", "mmhc"), autoLearnWithMWSTKnownBN, currentDirectory = "alarm", numIterations = 20)

# k2Matlab -> k2 bnlearn using k2Matlab2bnLearn
# tetrad matrix to pdag matrix
tetrad2Std("asia")
tetrad2Std("sachs")
tetrad2Std("insurance")
tetrad2Std("alarm")

# evaluation using kld
sapply(c("aic", "bic", "bde", "k2", "mmhc", "k2Matlab", "pc"), autoKLDKnownBN, currentDirectory = "asia")
sapply(c("aic", "bic", "bde", "k2", "mmhc", "k2Matlab", "pc"), autoKLDKnownBN, currentDirectory = "sachs")
sapply(c("aic", "bic", "bde", "k2", "mmhc", "k2Matlab", "pc"), autoKLDKnownBN, currentDirectory = "insurance")
sapply(c("aic", "bic", "bde", "k2", "mmhc", "k2Matlab", "pc"), autoKLDKnownBN, currentDirectory = "alarm")

# integrate cluster results for bir and rmse

currentDirectory = "alarm"

getMeanReferenceKnownBN(currentDirectory, 20)

sapply(c("aic", "bic", "bde", "k2", "mmhc", "k2Matlab", "pc"), getMeanPredictionKnownBN, currentDirectory = currentDirectory, measure = "rmse", 
       numIterations = 20, alpha = 0.05)

sapply(c("aic", "bic", "bde", "k2", "mmhc", "k2Matlab", "pc"), getMeanPredictionKnownBN, currentDirectory = currentDirectory, measure = "bir", 
       numIterations = 20, alpha = 0.05)

randomGuessingRMSEsKnownBN(currentDirectory, 20)

####################################### plot ####################################
# save plots
numIterations = 20
currentDirectory = "alarm"

measure = "precision"
measure = "recall"
measure = "f measure"
measure = "shd cpdags"
measure = "shd dags"
measure = "shd skeletons"
measure = "rmse"
measure = "bir"
measure = "kld"

df_k2 = read.csv(paste0("Evaluations/", knownBN, "/", measure, "/stats/stats_k2.csv"), header = TRUE)
df_mmhc = read.csv(paste0("Evaluations/", knownBN, "/", measure, "/stats/stats_mmhc.csv"), header = TRUE)
df_aic = read.csv(paste0("Evaluations/", knownBN, "/", measure, "/stats/stats_aic.csv"), header = TRUE)
df_bic = read.csv(paste0("Evaluations/", knownBN, "/", measure, "/stats/stats_bic.csv"), header = TRUE)
df_bde = read.csv(paste0("Evaluations/", knownBN, "/", measure, "/stats/stats_bde.csv"), header = TRUE)
tempMean = cbind(numInstancesList, df_k2$average, df_mmhc$average, df_aic$average, df_bic$average, df_bde$average)
colnames(tempMean) = c("numInstances", "k2", "mmhc", "aic", "bic", "bde")
tempMean = as.data.frame(tempMean) # convert matrix to data.frame
meltTempMean = melt(tempMean, id = "numInstances")
error = c(df_k2$error, df_mmhc$error, df_aic$error, df_bic$error, df_bde$error) # standard error
figure = ggplot(meltTempMean, aes(x = meltTempMean[,1], y = value, color = variable)) + 
  geom_line(data = meltTempMean) +ylab(label = measure) + xlab("numInstances")
figure = figure + geom_errorbar(aes(ymin = value - error, ymax = value + error), width = 0.2)
figure

if(!(dir.exists(paste0("Plots/", knownBN)))) dir.create(paste0("Plots/", knownBN))
ggsave(filename = paste0("Plots/", knownBN, "/", measure, "_", knownBN, ".png"))
figure




