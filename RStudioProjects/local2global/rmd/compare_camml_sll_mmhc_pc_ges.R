
setwd("~/Documents/Experiments/test_other_bn_learners_09_july_2019/")
library(bnlearn)
library(wrsgraph)
library(igraph)
library(lglbnlearn)
library(gtools)
library(doParallel)
library(readr)
library(ggplot2)
library(reshape)
library(pcalg)
library(stringr)
# nvars = 50
# maxNPas = 5
# n = 300
# name = paste0(nvars, "_", maxNPas, "_2_1_", n)

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
  system(paste0("java -jar blip.jar scorer.is -d data_blip/", datasets[ii], " -j dag_others/kmax_blip_java/parentSets/", datasets[ii], " -t 60 -b 0"))
}

# run kmax from blip (in java) to learn a complete bn
paSets = list.files("dag_others/kmax_blip_java/parentSets/")
for (ii in 1:length(paSets)) {
  system(paste0("java -jar blip.jar solver.kmax -w 20 -j dag_others/kmax_blip_java/parentSets/", paSets[ii], " -r dag_others/kmax_blip_java/", paSets[ii], " -t 60 -b 0"))
}

################################################################################
################################################################################
dags = list.files("dag/", ".rds")
alg = "camml"
prior = "/mmlcpt/0.3/"
m1 = m2 = pre = rec = c()
for (i in 1:length(dags)) {
  name = strsplit(dags[i], ".rds")[[1]]
  if (alg == "camml") {
    files = list.files(paste0("dag_others/camml", prior), pattern = name)
  } else {
    files = list.files(paste0("dag_others/", alg), pattern = name)  
  }
  
  dag = readRDS(paste0("dag/", dags[i]))
  mr = bnlearn::moral(dag)
  mr = dag2matrix(mr)
  vars = bnlearn::nodes(dag)
  # m = matrix(0, ncol = length(dags), nrow = length(files))
  for (j in 1:length(files)) {
    if (alg == "camml") {
      dne = readr::read_file(paste0("dag_others/camml", prior, "/", files[j]))
      dag_learned = dne2bnlearn(dne)
    } else if (alg %in% c("sllg_cpp", "sllc_cpp")) {
      mtx = read.table(paste0("dag_others/", alg, "/", files[j]))
      colnames(mtx) = rownames(mtx) = vars
      dag_learned = matrix2dag(mtx)
    } else if (alg == "kmax_blip_java") {
      dag_learned = blip2bnlearn(paste0("dag_others/", alg, "/", files[j]), vars)
    } else {
      dag_learned = readRDS(paste0("dag_others/", alg, "/", files[j]))
    }
    m1 = c(m1, bnlearn::shd(dag_learned, dag))
    mr_learned = dag2matrix(bnlearn::moral(dag_learned))
    res = edit_dist_graph(mr_learned, mr)
    m2 = c(m2, res$ed)
    pre = c(pre, res$pre)
    rec = c(rec, res$rec)
  }
}
mean(m1)
1.96*sd(m1)/10
mean(m2)
1.96*sd(m2)/10
mean(pre)
mean(rec)



