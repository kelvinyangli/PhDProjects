# re-learn with 100 samples but reverse the true ordering 
# this is to justify that ordering plays an important role when samples are not sufficient to learn structures
# datasets with only 100 samples

##############################################################################################################
datasets = list.files("numInstances/Datasets/Training/", pattern = "30_4_3_20_100_")

# corresponds true cpts
listOfcptsTrue = list.files("numInstances/True networks/CPTs/", pattern = "30_4_3_20_100_")

for (i in 1:length(datasets)) {
  
  data = readRDS(paste0("numInstances/Datasets/Training/", datasets[i]))
  
  cptsTrue = readRDS(paste0("numInstances/True networks/CPTs/", listOfcptsTrue[i]))
  
  # reverse the correct ordering of true dag  
  orderingReversed = rev(bnlearn::nodes(cptsTrue))
  
  # re-order data
  data = data[, orderingReversed]
  
  # learn structures
  methods = c("aic", "bic", "bde", "k2", "mmhc")
  
  for (j in 1:length(methods)) {
    
    if (methods[j] == "mmhc") {
      
      dagLearned = mmhc(data, test = "x2", score = "bde")
      
    } else {
      
      dagLearned = hc(data, score = methods[j])
      
    }
    
    # estimate cpts
    cptsLearned = bn.fit(dagLearned, data, method = "bayes")
    
    saveRDS(cptsLearned, paste0("numInstances/Learned networks/CPTs_100 samples/", methods[j], "/", datasets[i]))
    
  } # end for j
  
} # end for i

##############################################################################################################
# evaluation
methods = c("aic", "bic", "bde", "k2", "mmhc")

allTrueDagsList = list()

for (i in 1:length(listOfcptsTrue)) {

  allTrueDagsList[[i]] = model2network(modelstring(readRDS(paste0("numInstances/True networks/CPTs/", listOfcptsTrue[i]))))

}

sapply(methods, autoEditDistance, currentDirectory = "numInstances_100", numIterations = 20, allTrueDagsList = allTrueDagsList)

##############################################################################################################

ls = 100

df_mmhc = read.csv(paste0(currentDirectory, "/Evaluations/", measure, "/stats/stats_mmhc.csv"), header = TRUE)
df_aic = read.csv(paste0(currentDirectory, "/Evaluations/", measure, "/stats/stats_aic.csv"), header = TRUE)
df_bic = read.csv(paste0(currentDirectory, "/Evaluations/", measure, "/stats/stats_bic.csv"), header = TRUE)
df_bde = read.csv(paste0(currentDirectory, "/Evaluations/", measure, "/stats/stats_bde.csv"), header = TRUE)
df_k2 = read.csv(paste0(currentDirectory, "/Evaluations/", measure, "/stats/stats_k2.csv"), header = TRUE)

tempMean = cbind(ls, df_aic$average, df_bic$average, df_bde$average, df_k2$average, df_mmhc$average)

colnames(tempMean) = c("sample size", methods)


tempMean = as.data.frame(tempMean) # convert matrix to data.frame
meltTempMean = melt(tempMean, id = currentDirectory)
colnames(meltTempMean)[2] = "Methods"
error = c(df_pc$error, df_mmhc$error, df_aic$error, df_bic$error, df_bde$error, df_k2$error, df_k2Matlab$error,
          df_best$error, df_worst$error, df_true$error) # standard error

if (measure == "f measure") {
  ylabel = "F measure"
} else if (measure == "precision") {
  ylabel = "Precision"
} else if (measure == "recall") {
  ylabel = "Recall"
} else if (measure == "shd cpdags") {
  ylabel = "Edit distance for CPDAG"
} else if (measure == "shd dags") {
  ylabel = "Edit distance for DAG"
} else if (measure == "rmse") {
  ylabel = "Root mean square error"
} else if (measure == "bir") {
  ylabel = "Bayesian information reward"
} else if (measure %in% c("kld", "approx kld")) {
  ylabel = "Constant - KL-divergece"
} else if (measure == "density") {
  ylabel = "Average density"
}

figure = ggplot(meltTempMean, aes(x = meltTempMean[,1], y = value, color = Methods)) + 
  geom_line(data = meltTempMean) + ylab(label = ylabel) + xlab(xlabel)

figure = figure + geom_errorbar(aes(ymin = value - error, ymax = value + error), width = 0.2) + 
  ylim(min(meltTempMean$value-error), max(meltTempMean$value+error))

figure

ggsave(figure, filename = paste0(currentDirectory, "/Plots/", measure, "_", currentDirectory, ".png"),
       width = 20, height = 10, unit = "cm")

ggsave(figure, filename = paste0("C:/Users/Administrator/Dropbox/PhD@Monash/Experimental report/figures/", measure, "_", currentDirectory, ".png"),
       width = 20, height = 10, unit = "cm")
