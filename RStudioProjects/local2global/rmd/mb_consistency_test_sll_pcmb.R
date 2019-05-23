# test sll, iamb, pcmb on mb consistency
# 30 vars, maxNPa 5, binary, beta = 1
# generate random 10 dags+cpts, each then generate 10 datasets
# so in total 100 datasets
# sample size varies from 100, 1000, 10000
# test iamb, pcmb, sll and mml on these datasets
# use the authors default parameter settings
# loop through all variables then form a undirected graph
# test morality using backtracking
# set excution time limit to 5 mins, if running time too long then skip
# report percentage of consistent mbs for each method
#

setwd("~/Documents/Experiments/mb_consistency_test_16_may_2019/")
library(bnlearn)
library(wrsgraph)
library(igraph)
library(lglbnlearn)
library(gtools)
name = "30_5_2_1"
ndags = 10
ndatasets = 10
for (i in 1:ndags) {
  seed1 = randSeed()
  dag = randDag(30, 5)
  cpts = randCPTs(dag, 2, 1)
  saveRDS(dag, paste0("dag/", name, "_", seed1, ".rds"))
  saveRDS(cpts, paste0("cpts/", name, "_", seed1, ".rds"))
  for (n in c(100, 1000, 10000)) {
    for (j in 1:ndatasets) {
      seed2 = randSeed()
      data = rbn(cpts, n)
      data = factor2numeric(data)
      write.table(data, paste0("data_sll/", n, "/", name, "_", seed1, "_", seed2), row.names = F, col.names = F)
      write.table(data, paste0("data_pcmb/", n, "/", name, "_", seed1, "_", seed2), row.names = F)
    }
  }
}

# learn mb using sll
n = 5000
# dir = "~/Documents/Experiments/mb_consistency_test_16_may_2019/"
# dts = list.files(paste0(dir, "data_sll/", n))
# setwd("~/Documents/Experiments/SLL-1.0/")
# for (i in 1:length(dts)) {
#   system(paste0("./sll ", dir, "data_sll/", n, "/", dts[i],
#                 " -a sll-mb --output-mb-file ", dir, "mb_sll/", n, "/", dts[i]))
# }

# convert sll mb list into adj mtx
mbs = list.files(paste0(dir, "mb_sll/", n))
moral = rep(0, length(mbs))
sym = rep(0, length(mbs))
nedges = moral
for (j in 1:length(mbs)) {
  mb = read.table(paste0(dir, "mb_sll/", n, "/", mbs[j]), sep = "\n")
  mtx = matrix(0, nrow = 30, ncol = 30)
  sym[j] = isSymmetric(mtx)
  for (i in 1:30) {
    temp = strsplit(as.character(mb[i, 1]), ": ")[[1]][-1]
    if (length(temp) > 0) {
      ind = as.numeric(strsplit(temp, " ")[[1]]) + 1
      mtx[i, ind] = 1
    }
  }
  nedges[j] = sum(mtx) / 2
  # test morality
  moral[j] = wrsgraph::wrs_bktr(mtx, mtx)$wrs
}
sum(moral)
sum(sym)
mean(nedges)
#nedges
# n=100, 12%, avg 35.25 edges;
# n=1000, 0%, avg 55.91 edges;
# n=5000, 0%, avg 92 edges, but only tested on 10 datasets due to time;

# learn mb using pcmb
n = 5000
dts = list.files(paste0(dir, "data_pcmb/", n))
setwd("~/Documents/Experiments/mb_consistency_test_16_may_2019/data_pcmb/")
for (i in 1:length(dts)) {
  mb_pcmb = system(paste0("./kmb4_linux ", n, "/", dts[i], " ", n, " 30 -1 1.0 0 0 0.01"), intern = TRUE) # call c++ pcmb
  mb_pcmb = pcmb2mtx(mb_pcmb, "OR")
  write.table(mb_pcmb, paste0("~/Documents/Experiments/mb_consistency_test_16_may_2019/mb_iamb/", n, "/", dts[i]), row.names = F, col.names = F)
}

# test morality
n = 100
mbs = list.files(paste0(dir, "mb_iamb/", n))
sym = moral = nedges = rep(0, length(mbs))
for (j in 1:length(mbs)) {
  mtx = read.table(paste0(dir, "mb_iamb/", n, "/", mbs[j]))
  mtx = as.matrix(mtx)
  colnames(mtx) = c()
  sym[j] = as.numeric(isSymmetric(mtx))
  nedges[j] = sum(mtx) / 2
  # test morality
  moral[j] = wrsgraph::wrs_bktr(mtx, mtx)$wrs
}
sum(sym)
sum(moral)
mean(nedges)
# n=100, 92%, avg 14.77 edges;
# n=1000, 3%, avg 44.86 edges;
# n=5000, 0%, avg 97.36 edges but only tested on 14 datasets due to time;

# iamb 
# n=100, 0.07, avg 34.15
# n=1000, 0, 70.91
# n=5000, 0, 97.48
# 
pcmb2mtx = function(output, rule = c("AND", "OR")) {
  nvars = length(output) - 1
  mtx = matrix(0, nrow = nvars, ncol = nvars)
  lst = list()
  for (i in 1:nvars) {
    temp = output[i + 1]
    ind = strsplit(temp, "mb:")[[1]][2]
    if (!is.na(ind)) {
      cha = strsplit(ind, ",")[[1]]
      cha = cha[-length(cha)]
      lst[[i]] = as.numeric(cha) + 1
    } else {
      lst[[i]] = vector(length=0)
    }
  }

  for (i in 1:nvars) {
    if (length(lst[[i]]) > 0) {
      for (j in lst[[i]]) {
        if (rule == "AND") {
          if (i %in% lst[[j]]) mtx[i, j] = 1
        } else if (rule == "OR") {
          mtx[i, j] = mtx[j, i] = 1
        }

      }
    }

  }
  return(mtx)

}

