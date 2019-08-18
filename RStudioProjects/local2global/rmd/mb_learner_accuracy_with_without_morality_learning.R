# test mb accuracy w/ and w/o morality
# use min deg, min deficiency and true order
library(bnlearn)
library(doParallel)
library(wrsgraph)
library(lglbnlearn)
library(gtools)
library(xlsx)

setwd("~/Documents/Experiments/mb_learner_accuracy_with_without_morality_26_july_2019/")
nvars = 50
maxNPas = 5
n = 100*2^10
name = paste0(nvars, "_", maxNPas, "_2_1_", n)
# dir.create("dag", showWarnings = F)
# dir.create("cpts", showWarnings = F)
# dir.create("data_rds", showWarnings = F)

################################################################################
# generate random models and data
################################################################################
# for (i in 1:20) {
#   sd1 = randSeed()
#   set.seed(sd1)
#   dag_true = randDag(nvars, maxNPas)
#   cpts = randCPTs(dag_true, 2, 1)
#   saveRDS(dag_true, paste0("dag/",name, "_", sd1, ".rds"))
#   saveRDS(cpts, paste0("cpts/",name, "_", sd1, ".rds"))
#   for (j in 1:10) {
#     sd2 = randSeed()
#     set.seed(sd2)
#     data = rbn(cpts, n)
#     saveRDS(data, paste0("data_rds/", name, "_", sd1, "_", sd2, ".rds"))
#   }
# }

# for (i in 1:10) {
#   sd1 = randSeed()
#   set.seed(sd1)
#   dag_true = randDag(nvars, maxNPas)
#   saveRDS(dag_true, paste0("dag/",name, "_", sd1, ".rds"))
#   for (k in 1:10) {
#     sd3 = randSeed()
#     set.seed(sd3)
#     cpts = randCPTs(dag_true, 2, 1)  
#     saveRDS(cpts, paste0("cpts/",name, "_", sd1, "_", sd3, ".rds"))
#     
#     for (j in 1:1) {
#       sd2 = randSeed()
#       set.seed(sd2)
#       data = rbn(cpts, n)
#       saveRDS(data, paste0("data_rds/", name, "_", sd1, "_", sd3, "_", sd2, ".rds"))
#     }
#   }
# }

################################################################################
# get dag stats 
################################################################################
# dags = list.files("dag")
# dagStats = matrix(ncol = length(dags), nrow = 4)
# alg = "pcmb"
# row.names(dagStats) = c("nnodes", "narcs", "maxMB", "meanMB")
# for (i in 1:length(dags)) {
#   dag = readRDS(paste0("dag/", dags[i]))
#   dagStats[1, i] = nnodes(dag)
#   dagStats[2, i] = narcs(dag)
#   vars = nodes(dag)
#   mbsizes = sapply(lapply(vars, bnlearn::mb, x = dag), length)
#   dagStats[3, i] = max(mbsizes)
#   dagStats[4, i] = mean(mbsizes)
# }
# t(dagStats)
# write.table(t(dagStats), paste0("results_", alg, ".txt"), append = T)
# write.table("***", paste0("results_", alg, ".txt"), append = T, row.names = F, col.names = F)

################################################################################
# mb and ordering learning
################################################################################
# use a fast str learner (mmhc) to approximate a node ordering
# save mmch learned bn into disk
# later when needed, node ordering can be extracted easily
dts = list.files("data_rds/")
for (nn in c(3200,6400,12800)) {
  dir.create(paste0("mmhc_", nn), showWarnings = F)
  dir.create(paste0("mb_mmlcpt_", nn), showWarnings = F)
  dir.create(paste0("data_rds_", nn), showWarnings = F)
  for (i in 1:length(dts)) {
    data = readRDS(paste0("data_rds/", dts[i]))
    n = nrow(data)
    # sample data with size nn
    data = data[sample(n, nn, replace = F), ]
    saveRDS(data, paste0("data_rds_", nn, "/", dts[i]))

    # learn bn using mmhc
    x = mmhc(data)
    saveRDS(x, paste0("mmhc_", nn, "/", dts[i]))

    # learn mb using mmlcpt
    vars = colnames(data)
    arities = rep(2, 50)
    di = count_occurance(data, arities)

    doParallel::registerDoParallel(4)
    mbcpt = foreach(target = vars,
                    .combine = list,
                    .multicombine = TRUE) %dopar% {
                      forward_greedy_fast(data, di, arities, vars, nn, target)
                    }
    stopImplicitCluster()
    names(mbcpt) = vars
    saveRDS(mbcpt, paste0("mb_mmlcpt_", nn, "/", dts[i]))
  }
}

# learning using iamb 
for (nn in c(3200,6400,12800)) {
  dir.create(paste0("mb_iamb_", nn), showWarnings = F)
  dts = list.files(paste0("data_rds_", nn))
  for (i in 1:length(dts)) {
    data = readRDS(paste0("data_rds_", nn, "/", dts[i]))
    vars = names(data)
    mbIAMB = lapply(vars, learn.mb, x = data, method = "iamb", alpha = 0.01)
    names(mbIAMB) = vars
    saveRDS(mbIAMB, paste0("mb_iamb_", nn, "/", dts[i]))
  }
}

################################################################################
# generate random node orders
################################################################################
# dags = list.files("dag/", pattern = ".rds")
# for (i in 1:length(dags)) {
#   dag = readRDS(paste0("dag/", dags[i]))
#   name = strsplit(dags[i], ".rds")[[1]][1]
#   ord = node.ordering(dag)
#   for (p in seq(0, 1, 0.1)) {
#     ord2 = random_permutation_with_randomness(ord, p) 
#     saveRDS(ord2, paste0("node_orders_random/", name, "_", p, ".rds"))
#   }
# }

################################################################################
# learn ordering using mwst
################################################################################
for (nn in c(400,800,1600,3200,6400,12800)) {
  dir.create(paste0("chow_liu_", nn), showWarnings = F)
  dts = list.files(paste0("data_rds_", nn))
  for (i in 1:length(dts)) {
    data = readRDS(paste0("data_rds_", nn, "/", dts[i]))
    x = chow.liu(data)
    saveRDS(x, paste0("chow_liu_", nn, "/", dts[i]))
  }
}



