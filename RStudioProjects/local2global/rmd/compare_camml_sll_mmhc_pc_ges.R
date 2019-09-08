
setwd("~/Documents/Experiments/test_other_bn_learners_08_july_2019/random_parameter_dirichlet_0.2/")
library(bnlearn)
library(wrsgraph)
library(igraph)
library(mbmml)
library(gtools)
library(doParallel)
library(readr)
library(ggplot2)
library(reshape)
library(pcalg)
library(stringr)
nvars = 50
maxNPas = 5
n = 300
name = paste0(nvars, "_", maxNPas, "_2_1_", n)

getArity = function(cpts, x) {
  a = dim(cpts[[x]]$prob)[1]
  return(a)
}

# arities of real modesl 
arity_list = list()
files = list.files("original_parameters/cpts/")
for (i in 1:length(files)) {
  cpts = readRDS(paste0("original_parameters/cpts/", files[i]))
  vars = nodes(cpts)
  arities = sapply(vars, getArity, cpts = cpts)
  names(arities) = c()
  arity_list[[i]] = arities
}

# random cpts
dags = list.files("dag/")
for (i in 1:length(dags)) {
  dag = readRDS(paste0("dag/", dags[i]))
  name = strsplit(dags[i], ".rds")[[1]][1]
  for (j in 1:1) {
    sd = randSeed()
    set.seed(sd)
    # cpts = randCPTs(dag, 2, 0.2, arities = arity_list[[i]])
    cpts = randCPTs(dag, 2, 0.2, arities = rep(2, 50))
    saveRDS(cpts, paste0("cpts/", name, "_", sd, ".rds"))
  } 
}

# random data
files = list.files("cpts/")
for (i in 1:length(files)) {
  cpts = readRDS(paste0("cpts/", files[i]))
  name = strsplit(files[i], ".rds")[[1]][1]
  for (j in 1:10) {
    sd = randSeed()
    set.seed(sd)
    data = rbn(cpts, n)
    saveRDS(data, paste0("data_rds/", name, "_", sd, ".rds"))
  }
}

# data for camml
dts = list.files("data_rds/")
for (i in 1:length(dts)) {
  name = strsplit(dts[i], ".rds")[[1]][1]
  data = readRDS(paste0("data_rds/", dts[i]))
  write.csv(data, paste0("data_csv/", name, ".csv"), row.names = F)
}
########################### mmhc and pc in bnlearn #############################
################################################################################
dts = list.files("data_rds/")
for (i in 1:length(dts)) {
  data = readRDS(paste0("data_rds/", dts[i]))
  dag_mmhc = mmhc(data)
  saveRDS(dag_mmhc, paste0("dag_others/mmhc_bnlearn/", dts[i]))

  dag_pc = pc.stable(data, alpha = 0.05)
  saveRDS(dag_pc, paste0("dag_others/pc_0.05_bnlearn/", dts[i]))
}
################################################################################
################################################################################


##################################### sll ######################################
################################################################################
dts = list.files("data_rds/")
# prepare data to sll required format
for (i in 1:length(dts)) {
  data = readRDS(paste0("data_rds/", dts[i]))
  data = factor2numeric(data)
  name = strsplit(dts[i], ".rds")[[1]][1]
  write.table(data, paste0("data_sll/", name), row.names = F, col.names = F)
}

#setwd("~/Documents/Experiments/SLL-1.0/")
datasets = list.files("data_sll/")
for (ii in 1:length(datasets)) {
  system(paste0("~/Documents/Experiments/SLL-1.0/sll data_sll/", datasets[ii], " -a sll+g --output-dag-file dag_others/sllg_cpp/", datasets[ii]))
  system(paste0("~/Documents/Experiments/SLL-1.0/sll data_sll/", datasets[ii], " -a sll+c --output-dag-file dag_others/sllc_cpp/", datasets[ii]))
}
################################################################################
################################################################################

############################### ges in pcalg ###################################
################################################################################
# ges in pcalg
# at the moment it isn't working
data(gmG)

## Define the score (BIC)
score <- new("GaussL0penObsScore", gmG8$x)

## Estimate the essential graph
ges.fit <- ges(score)

# a function to convert pcalg dag format to bnlearn dag format
# x is a dag object
pcalg2bnlearn = function(x) {
  nodes = x$.nodes
  dg = bnlearn::empty.graph(nodes)
  for (i in 1:x$edge.count()) {
    pa = x$.in.edges[[i]]
    if (length(pa) > 0) {
      for (j in pa) {
        dg = bnlearn::set.arc(dg, nodes[j], nodes[i])
      }

    }
  }
  return(dg)
}
################################################################################
################################################################################


######################## bounded treewidth str optimization in blip ############
################################################################################
# source code is in java
# first prepare data set
dts = list.files("data_rds/")
for (i in 1:length(dts)) {
  data = readRDS(paste0("data_rds/", dts[i]))
  arities = rep(2, ncol(data))
  for (j in 1:ncol(data)) arities[j] = nlevels(data[, j])
  # nodes = colnames(data)
  data = factor2numeric(data)
  data = rbind(arities, data)
  name = strsplit(dts[i], ".rds")[[1]][1]
  write.table(data, paste0("data_blip/", name), row.names = F)
}

# a function to convert dlip format dag to
blip2bnlearn = function(filePath, vars) {
  res_blip = read.table(filePath, sep = "\n") # read into r as a data.frame?
  dg = bnlearn::empty.graph(vars)
  for (i in 1:(nrow(res_blip) - 1)) {
    # get the parenthesis and what is inside
    k = str_extract_all(res_blip[i,], "\\([^()]+\\)")[[1]]
    # remove parenthesis
    k = substring(k, 2, nchar(k) - 1)
    if (length(k) > 0) {
      indx = as.numeric(strsplit(k, ",")[[1]]) + 1 # add 1 since index starts from 0
      for (j in indx) {
        dg = set.arc(dg, vars[j], vars[i])
      }
    }
  }
  return(dg)
}

# run kmax from blip (in java) to learn parent sets
datasets = list.files("data_blip/")
for (ii in 1:length(datasets)) {
  system(paste0("java -jar blip.jar scorer.is -d data_blip/", datasets[ii], " -j dag_others/kmax_blip_java/parentSets/", datasets[ii], " -t 5 -b 0"))
}

# run kmax from blip (in java) to learn a complete bn
paSets = list.files("dag_others/kmax_blip_java/parentSets/")
for (ii in 1:length(datasets)) {
  system(paste0("java -jar blip.jar solver.kmax -w 20 -j dag_others/kmax_blip_java/parentSets/", paSets[ii], " -r dag_others/kmax_blip_java/", paSets[ii], " -t 5 -b 0"))
}

################################################################################
################################################################################
dags = list.files("dag/", ".rds")
alg = "kmax_blip_java"
ed = pre = rec = c()
for (i in 1:length(dags)) {
  
  name = strsplit(dags[i], ".rds")[[1]]
  dag = readRDS(paste0("dag/", dags[i]))
  g = dag2matrix(dag)
  # mr = bnlearn::moral(dag)
  # mr = dag2matrix(mr)
  vars = bnlearn::nodes(dag)
  
  if (alg == "kmax_blip_java") {
    files = list.files(paste0("dag_others/", alg, "/tw20"), pattern = name)
  } else {
    files = list.files(paste0("dag_others/", alg), pattern = name)  
  }
  
  for (j in 1:length(files)) {
    if (alg == "camml") {
      dne = readr::read_file(paste0("dag_others/camml/", files[j]))
      dag_learned = dne2bnlearn(dne)
    } else if (alg %in% c("sllg_cpp", "sllc_cpp")) {
      mtx = read.table(paste0("dag_others/", alg, "/", files[j]))
      colnames(mtx) = rownames(mtx) = vars
      dag_learned = matrix2dag(mtx)
    } else if (alg == "kmax_blip_java") {
      dag_learned = blip2bnlearn(paste0("dag_others/", alg, "/tw20/", files[j]), vars)
    } else {
      dag_learned = readRDS(paste0("dag_others/", alg, "/", files[j]))
    }
  
    g_learned = dag2matrix(dag_learned)
    res = edit_dist_dag(g_learned, g)
    ed = c(ed, res$ed)
    pre = c(pre, res$pre)
    rec = c(rec, res$rec)
  }
  
}

# mean(ed)
# 1.96*sd(ed)/sqrt(length(ed))
# mean(pre)
# 1.96*sd(pre)/sqrt(length(pre))
# mean(rec)
# 1.96*sd(rec)/sqrt(length(rec))

for (i in 1:6) {
  cat(mean(ed[((i-1)*20+1):(i*20)]), "\n")
  cat(1.96*sd(ed[((i-1)*20+1):(i*20)])/sqrt(20), "\n")  
}



