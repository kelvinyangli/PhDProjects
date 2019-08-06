# test mb accuracy w/ and w/o morality
# use min deg, min deficiency and true order
library(bnlearn)
library(doParallel)
library(wrsgraph)
library(lglbnlearn)
library(gtools)

setwd("~/Documents/Experiments/mb_learner_accuracy_with_without_morality_26_july_2019/")
nvars = 50
maxNPas = 5
n = 100*2^10
name = paste0(nvars, "_", maxNPas, "_2_1_", n)
dir.create("dag", showWarnings = F)
dir.create("cpts", showWarnings = F)
dir.create("data_rds", showWarnings = F)

################################################################################
# generate random models and data
################################################################################
for (i in 1:20) {
  sd1 = randSeed()
  set.seed(sd1)
  dag_true = randDag(nvars, maxNPas)
  cpts = randCPTs(dag_true, 2, 1)
  saveRDS(dag_true, paste0("dag/",name, "_", sd1, ".rds"))
  saveRDS(cpts, paste0("cpts/",name, "_", sd1, ".rds"))
  for (j in 1:10) {
    sd2 = randSeed()
    set.seed(sd2)
    data = rbn(cpts, n)
    saveRDS(data, paste0("data_rds/", name, "_", sd1, "_", sd2, ".rds"))
  }
}

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
dags = list.files("dag")
dagStats = matrix(ncol = length(dags), nrow = 4)
alg = "pcmb"
row.names(dagStats) = c("nnodes", "narcs", "maxMB", "meanMB")
for (i in 1:length(dags)) {
  dag = readRDS(paste0("dag/", dags[i]))
  dagStats[1, i] = nnodes(dag)
  dagStats[2, i] = narcs(dag)
  vars = nodes(dag)
  mbsizes = sapply(lapply(vars, bnlearn::mb, x = dag), length)
  dagStats[3, i] = max(mbsizes)
  dagStats[4, i] = mean(mbsizes)
}
t(dagStats)
write.table(t(dagStats), paste0("results_", alg, ".txt"), append = T)
write.table("***", paste0("results_", alg, ".txt"), append = T, row.names = F, col.names = F)

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
# evaluation on different qualities of node ordering
################################################################################
dags = list.files("dag/", pattern = ".rds")
alg = "pcmb"
for (nn in c(3200,6400,12800)) { # no morality 

for (p in seq(0, 1, 0.1)) {
mbed = pre = rec = c()
mbed1 = pre1 = rec1 = c() # mini deg
mbed2 = pre2 = rec2 = c() # mini deficiency
mbed3 = pre3 = rec3 = c() # mmhc node ordering
mbed4 = pre4 = rec4 = c() # a true ordering from the true dag 
mbed5 = pre5 = rec5 = c() # mmhc moral graph accuracy 
mbed6 = pre6 = rec6 = c() # mmhc node ordering, triangulation
mbed7 = pre7 = rec7 = c() # a true ordering from the true dag, triangulation 

for (i in 1:length(dags)) {
  name = strsplit(dags[i], ".rds")[[1]]
  # mmhc learnd dag and mb_mmlcpt and data have the same name
  files = list.files(paste0("mb_", alg, "_", nn), pattern = name)
  if (length(files) == 0) break
  dag = readRDS(paste0("dag/", dags[i]))
  vars = bnlearn::nodes(dag)
  mr = dag2matrix(bnlearn::moral(dag))
  ord = rev(bnlearn::node.ordering(dag))
  for (j in 1:length(files)) {
    ord2 = random_permutation_with_randomness(ord, p)
    mbcpt = readRDS(paste0("mb_", alg, "_", nn, "/", files[j]))
    if (alg == "pcmb") {
      mbcpt_sym = symmetry_correction(vars, mbcpt, "intersection")
    } else {
      mbcpt_sym = symmetry_correction(vars, mbcpt, "union")
    }
    G = mblist2moral(mbcpt_sym, vars)
    # enforce morality
    G1 = wrsgraph::min_deg_moralization(G)
    G2 = wrsgraph::min_deficiency_moralization(G)
    G3 = wrsgraph::fixed_ordering_moralization(G, ord2)
    G4 = wrsgraph::fixed_ordering_moralization(G, ord)
    G6 = moralization(G, ord2, 0)
    G7 = moralization(G, ord, 0)
    
    # k = k + 1
    res = edit_dist_graph(G, mr)
    mbed = c(mbed, res$ed)
    pre = c(pre, res$pre)
    rec = c(rec, res$rec)

    res1 = edit_dist_graph(G1, mr)
    mbed1 = c(mbed1, res1$ed)
    pre1 = c(pre1, res1$pre)
    rec1 = c(rec1, res1$rec)

    res2 = edit_dist_graph(G2, mr)
    mbed2 = c(mbed2, res2$ed)
    pre2 = c(pre2, res2$pre)
    rec2 = c(rec2, res2$rec)

    res3 = edit_dist_graph(G3, mr)
    mbed3 = c(mbed3, res3$ed)
    pre3 = c(pre3, res3$pre)
    rec3 = c(rec3, res3$rec)

    res4 = edit_dist_graph(G4, mr)
    mbed4 = c(mbed4, res4$ed)
    pre4 = c(pre4, res4$pre)
    rec4 = c(rec4, res4$rec)

    # res5 = edit_dist_graph(G5, mr)
    # mbed5 = c(mbed5, res5$ed)
    # pre5 = c(pre5, res5$pre)
    # rec5 = c(rec5, res5$rec)

    res6 = edit_dist_graph(G6, mr)
    mbed6 = c(mbed6, res6$ed)
    pre6 = c(pre6, res6$pre)
    rec6 = c(rec6, res6$rec)
    
    res7 = edit_dist_graph(G7, mr)
    mbed7 = c(mbed7, res7$ed)
    pre7 = c(pre7, res7$pre)
    rec7 = c(rec7, res7$rec)
  }
}

write.table(paste0(nvars, "_", maxNPas, "_2_1_", nn, "_", p), paste0("results_", alg, ".txt"), append = T, row.names = F, col.names = F)
# no moral 
x = round(c(mean(mbed), 1.96*sd(mbed)/sqrt(length(mbed)), mean(pre), mean(rec)), 2)
write.table(matrix(x, nrow = 1), paste0("results_", alg, ".txt"), append = T, row.names = F, col.names = F)

# mini deg 
x1 = round(c(mean(mbed1), 1.96*sd(mbed1)/sqrt(length(mbed1)), mean(pre1), mean(rec1)), 2)
write.table(matrix(x1, nrow = 1), paste0("results_", alg, ".txt"), append = T, row.names = F, col.names = F)

# mini deficiency
x2 = round(c(mean(mbed2), 1.96*sd(mbed2)/sqrt(length(mbed2)), mean(pre2), mean(rec2)), 2)
write.table(matrix(x2, nrow = 1), paste0("results_", alg, ".txt"), append = T, row.names = F, col.names = F)

# mmhc ordering, moralization
x3 = round(c(mean(mbed3), 1.96*sd(mbed3)/sqrt(length(mbed3)), mean(pre3), mean(rec3)), 2)
write.table(matrix(x3, nrow = 1), paste0("results_", alg, ".txt"), append = T, row.names = F, col.names = F)

# mmhc ordering, triangulation
x6 = round(c(mean(mbed6), 1.96*sd(mbed6)/sqrt(length(mbed6)), mean(pre6), mean(rec6)), 2)
write.table(matrix(x6, nrow = 1), paste0("results_", alg, ".txt"), append = T, row.names = F, col.names = F)

# true ordering, moralization
x4 = round(c(mean(mbed4), 1.96*sd(mbed4)/sqrt(length(mbed4)), mean(pre4), mean(rec4)), 2)
write.table(matrix(x4, nrow = 1), paste0("results_", alg, ".txt"), append = T, row.names = F, col.names = F)

# true ordering, triangulation
x7 = round(c(mean(mbed7), 1.96*sd(mbed7)/sqrt(length(mbed7)), mean(pre7), mean(rec7)), 2)
write.table(matrix(x7, nrow = 1), paste0("results_", alg, ".txt"), append = T, row.names = F, col.names = F)

write.table("***", paste0("results_", alg, ".txt"), append = T, row.names = F, col.names = F)
}

}


################################################################################
# evaluation on different approximated node ordering, eg mmhc, min deg, min def
################################################################################
dags = list.files("dag/", pattern = ".rds")
alg = "pcmb"
for (nn in c(3200,6400,12800)) { # no morality 
  
    mbed = pre = rec = c()
    mbed1 = pre1 = rec1 = c() # mini deg
    mbed2 = pre2 = rec2 = c() # mini deficiency
    mbed3 = pre3 = rec3 = c() # mmhc node ordering
    mbed4 = pre4 = rec4 = c() # a true ordering from the true dag 
    mbed5 = pre5 = rec5 = c() # mmhc moral graph accuracy 
    mbed6 = pre6 = rec6 = c() # mmhc node ordering, triangulation
    mbed7 = pre7 = rec7 = c() # a true ordering from the true dag, triangulation 
    
    for (i in 1:length(dags)) {
      name = strsplit(dags[i], ".rds")[[1]]
      # mmhc learnd dag and mb_mmlcpt and data have the same name
      files = list.files(paste0("mb_", alg, "_", nn), pattern = name)
      if (length(files) == 0) break
      dag = readRDS(paste0("dag/", dags[i]))
      vars = bnlearn::nodes(dag)
      mr = dag2matrix(bnlearn::moral(dag))
      ord = rev(bnlearn::node.ordering(dag))
      for (j in 1:length(files)) {
        dagMMHC = readRDS(paste0("mmhc_", nn, "/", files[j]))
        ord2 = rev(node.ordering(dagMMHC))
        mbcpt = readRDS(paste0("mb_", alg, "_", nn, "/", files[j]))
        if (alg == "pcmb") {
          mbcpt_sym = symmetry_correction(vars, mbcpt, "union")
        } else {
          mbcpt_sym = symmetry_correction(vars, mbcpt, "union")
        }
        G = mblist2moral(mbcpt_sym, vars)
        # enforce morality
        G1 = wrsgraph::min_deg_moralization(G)
        G2 = wrsgraph::min_deficiency_moralization(G)
        G3 = wrsgraph::fixed_ordering_moralization(G, ord2)
        G4 = wrsgraph::fixed_ordering_moralization(G, ord)
        G5 = dag2matrix(moral(dagMMHC))
        G6 = moralization(G, ord2, 0)
        G7 = moralization(G, ord, 0)
        
        # k = k + 1
        res = edit_dist_graph(G, mr)
        mbed = c(mbed, res$ed)
        pre = c(pre, res$pre)
        rec = c(rec, res$rec)
        
        res1 = edit_dist_graph(G1, mr)
        mbed1 = c(mbed1, res1$ed)
        pre1 = c(pre1, res1$pre)
        rec1 = c(rec1, res1$rec)
        
        res2 = edit_dist_graph(G2, mr)
        mbed2 = c(mbed2, res2$ed)
        pre2 = c(pre2, res2$pre)
        rec2 = c(rec2, res2$rec)
        
        res3 = edit_dist_graph(G3, mr)
        mbed3 = c(mbed3, res3$ed)
        pre3 = c(pre3, res3$pre)
        rec3 = c(rec3, res3$rec)
        
        res4 = edit_dist_graph(G4, mr)
        mbed4 = c(mbed4, res4$ed)
        pre4 = c(pre4, res4$pre)
        rec4 = c(rec4, res4$rec)
        
        res5 = edit_dist_graph(G5, mr)
        mbed5 = c(mbed5, res5$ed)
        pre5 = c(pre5, res5$pre)
        rec5 = c(rec5, res5$rec)
        
        res6 = edit_dist_graph(G6, mr)
        mbed6 = c(mbed6, res6$ed)
        pre6 = c(pre6, res6$pre)
        rec6 = c(rec6, res6$rec)
        
        res7 = edit_dist_graph(G7, mr)
        mbed7 = c(mbed7, res7$ed)
        pre7 = c(pre7, res7$pre)
        rec7 = c(rec7, res7$rec)
      }
    }
    
    write.table(paste0(nvars, "_", maxNPas, "_2_1_", nn, "_", p), paste0("results_", alg, ".txt"), append = T, row.names = F, col.names = F)
    # no moral 
    x = round(c(mean(mbed), 1.96*sd(mbed)/sqrt(length(mbed)), mean(pre), mean(rec)), 2)
    write.table(matrix(x, nrow = 1), paste0("results_", alg, ".txt"), append = T, row.names = F, col.names = F)
    
    # mini deg 
    x1 = round(c(mean(mbed1), 1.96*sd(mbed1)/sqrt(length(mbed1)), mean(pre1), mean(rec1)), 2)
    write.table(matrix(x1, nrow = 1), paste0("results_", alg, ".txt"), append = T, row.names = F, col.names = F)
    
    # mini deficiency
    x2 = round(c(mean(mbed2), 1.96*sd(mbed2)/sqrt(length(mbed2)), mean(pre2), mean(rec2)), 2)
    write.table(matrix(x2, nrow = 1), paste0("results_", alg, ".txt"), append = T, row.names = F, col.names = F)
    
    # mmhc ordering, moralization
    x3 = round(c(mean(mbed3), 1.96*sd(mbed3)/sqrt(length(mbed3)), mean(pre3), mean(rec3)), 2)
    write.table(matrix(x3, nrow = 1), paste0("results_", alg, ".txt"), append = T, row.names = F, col.names = F)
    
    # mmhc ordering, triangulation
    x6 = round(c(mean(mbed6), 1.96*sd(mbed6)/sqrt(length(mbed6)), mean(pre6), mean(rec6)), 2)
    write.table(matrix(x6, nrow = 1), paste0("results_", alg, ".txt"), append = T, row.names = F, col.names = F)
    
    # mmhc moral graph
    x5 = round(c(mean(mbed5), 1.96*sd(mbed5)/sqrt(length(mbed5)), mean(pre5), mean(rec5)), 2)
    write.table(matrix(x5, nrow = 1), paste0("results_", alg, ".txt"), append = T, row.names = F, col.names = F)
    
    # true ordering, moralization
    x4 = round(c(mean(mbed4), 1.96*sd(mbed4)/sqrt(length(mbed4)), mean(pre4), mean(rec4)), 2)
    write.table(matrix(x4, nrow = 1), paste0("results_", alg, ".txt"), append = T, row.names = F, col.names = F)
    
    # true ordering, triangulation
    x7 = round(c(mean(mbed7), 1.96*sd(mbed7)/sqrt(length(mbed7)), mean(pre7), mean(rec7)), 2)
    write.table(matrix(x7, nrow = 1), paste0("results_", alg, ".txt"), append = T, row.names = F, col.names = F)
    
    write.table("***", paste0("results_", alg, ".txt"), append = T, row.names = F, col.names = F)
  
}




















