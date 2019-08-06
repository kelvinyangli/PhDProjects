# test mb accuracy w/ and w/o morality
# use min deg, min deficiency and true order

# for (nvars in c(80, 70, 60, 40, 30, 20, 10)) {

setwd("~/Documents/Experiments/mb_learner_accuracy_with_without_morality_23_july_2019/")
nvars = 50
maxNPas = 5
n = 10000
name = paste0(nvars, "_", maxNPas, "_2_1_", n)
# create directory if it doesn't exist
dir.create(name, showWarnings = F)
setwd(name)
dir.create("dag", showWarnings = F)
dir.create("cpts", showWarnings = F)
dir.create("data_rds", showWarnings = F)
dir.create("mb_mmlcpt", showWarnings = F)

# generate random models and data
for (i in 1:10) {
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
    # write.csv(data, paste0("data_csv/", name, "_", sd1, "_", sd2, ".csv"), row.names = F)
  }
}

# learn mb using mmlcpt


# use a fast str learner to approximate a node ordering
# use mmhc
# orderingList = list()
# for (i in 1:length(dts)) {
#   data = readRDS(paste0("data_rds/", dts[i]))
#   x = bnlearn::mmhc(data)
#   orderingList[[i]] = rev(node.ordering(x))
# }

# setwd("50_5_2_1_5000/")
dts = list.files("data_rds/")
for (j in 26:length(dts)) {
  data = readRDS(paste0("data_rds/", dts[j]))
  # learn mb using mmlcpt
  vars = colnames(data)
  arities = sapply(data, nlevels)
  di = count_occurance(data, arities)
  
  # cl <- parallel::makeForkCluster(4)
  doParallel::registerDoParallel(4)
  mbcpt = foreach(target = vars,
                  .combine = list,
                  .multicombine = TRUE) %dopar% {
                    forward_greedy_fast(data, di, arities, vars, n, target)
                  }
  stopImplicitCluster()
  names(mbcpt) = vars
  saveRDS(mbcpt, paste0("mb_mmlcpt/", dts[j]))
}

# evaluation
mbed = pre = rec = c()
mbed1 = pre1 = rec1 = c()
mbed2 = pre2 = rec2 = c()
mbed3 = pre3 = rec3 = c()
# mbed4 = pre4 = rec4 = c()
dags = list.files("dag/", pattern = ".rds")
# k = 1
for (i in 1:length(dags)) {
  name = strsplit(dags[i], ".rds")[[1]]
  files = list.files("mb_mmlcpt", pattern = name)
  dag = readRDS(paste0("dag/", dags[i]))
  vars = bnlearn::nodes(dag)
  mr = dag2matrix(bnlearn::moral(dag))
  ord = rev(bnlearn::node.ordering(dag))
  for (j in 1:length(files)) {
    mbcpt = readRDS(paste0("mb_mmlcpt/", files[j]))
    mbcpt_sym = symmetry_correction(vars, mbcpt, "union")
    G = mblist2moral(mbcpt_sym, vars)
    # enforce morality
    G1 = wrsgraph::min_deg_moralization(G)
    G2 = wrsgraph::min_deficiency_moralization(G)
    G3 = wrsgraph::fixed_ordering_moralization(G, ord)
    # G4 = wrsgraph::fixed_ordering_moralization(G, orderingList[[k]])
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

    # res4 = edit_dist_graph(G4, mr)
    # mbed4 = c(mbed4, res4$ed)
    # pre4 = c(pre4, res4$pre)
    # rec4 = c(rec4, res4$rec)

  }
}

write.table(paste0(nvars, "_", maxNPas, "_2_1_", n), "../results.txt", append = T, row.names = F, col.names = F)
x = round(c(mean(mbed), 1.96*sd(mbed)/sqrt(length(mbed)), mean(pre), mean(rec)), 2)
write.table(matrix(x, nrow = 1), "../results.txt", append = T, row.names = F, col.names = F)

x1 = round(c(mean(mbed1), 1.96*sd(mbed1)/sqrt(length(mbed1)), mean(pre1), mean(rec1)), 2)
write.table(matrix(x1, nrow = 1), "../results.txt", append = T, row.names = F, col.names = F)

x2 = round(c(mean(mbed2), 1.96*sd(mbed2)/sqrt(length(mbed2)), mean(pre2), mean(rec2)), 2)
write.table(matrix(x2, nrow = 1), "../results.txt", append = T, row.names = F, col.names = F)

x3 = round(c(mean(mbed3), 1.96*sd(mbed3)/sqrt(length(mbed3)), mean(pre3), mean(rec3)), 2)
write.table(matrix(x3, nrow = 1), "../results.txt", append = T, row.names = F, col.names = F)

# x4 = round(c(mean(mbed4), 1.96*sd(mbed4)/sqrt(length(mbed4)), mean(pre4), mean(rec4)), 2)
# write.table(matrix(x4, nrow = 1), "results.txt", append = T, row.names = F, col.names = F)

# }


# learn mb using iamb 
setwd("../50_5_2_1_5000/")
dts = list.files("data_rds/")
dags = list.files("dag/")
mbed = pre = rec = c()
mbed1 = pre1 = rec1 = c()
mbed2 = pre2 = rec2 = c()
mbed3 = pre3 = rec3 = c()
for (i in 1:100) {
  k = ceiling(i/10)
  dag = readRDS(paste0("dag/", dags[k]))
  ord = rev(node.ordering(dag))
  mr = dag2matrix(bnlearn::moral(dag))
  data = readRDS(paste0("data_rds/", dts[i]))
  vars = colnames(data)
  mbcpt = lapply(vars, learn.mb, x = data, method = "iamb", alpha = 0.01)
  names(mbcpt) = vars
  mbcpt_sym = symmetry_correction(vars, mbcpt, "intersection")
  G = mblist2moral(mbcpt_sym, vars)
  # enforce morality
  G1 = wrsgraph::min_deg_moralization(G)
  G2 = wrsgraph::min_deficiency_moralization(G)
  G3 = wrsgraph::fixed_ordering_moralization(G, ord)
  # G4 = wrsgraph::fixed_ordering_moralization(G, orderingList[[k]])
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
}

mean(mbed)
1.96*sd(mbed)/sqrt(length(mbed))
mean(mbed1)
1.96*sd(mbed1)/sqrt(length(mbed1))
mean(mbed2)
1.96*sd(mbed2)/sqrt(length(mbed2))
mean(mbed3)
1.96*sd(mbed3)/sqrt(length(mbed3))

setwd("../../mb_discovery_IJAR/80_5_4_1/")
dags = list.files("dag/")

alg = "iamb_bnlearn_0.01/"
# alg = "mml_cpt/"
sym = TRUE
# rule = "intersection"
rule = "union"
files = list.files(alg, pattern = "_5000_")
mbed = pre = rec = c()
mbed1 = pre1 = rec1 = c()
mbed2 = pre2 = rec2 = c()
mbed3 = pre3 = rec3 = c()
for (i in 1:length(files)) {
  k = ceiling(i/5)
  dag = readRDS(paste0("dag/", dags[k]))
  ord = rev(node.ordering(dag))
  mr = dag2matrix(bnlearn::moral(dag))
  vars = bnlearn::nodes(dag)
  mbl = readRDS(paste0(alg, files[i]))
  names(mbl) = vars
  if (sym == TRUE) mbl = symmetry_correction(vars, mbl, rule)
  G = mblist2moral(mbl, vars)
  G1 = wrsgraph::min_deg_moralization(G)
  G2 = wrsgraph::min_deficiency_moralization(G)
  G3 = wrsgraph::fixed_ordering_moralization(G, ord)
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
}
mean(mbed)
1.96*sd(mbed)/sqrt(length(mbed))
mean(mbed1)
1.96*sd(mbed1)/sqrt(length(mbed1))
mean(mbed2)
1.96*sd(mbed2)/sqrt(length(mbed2))
mean(mbed3)
1.96*sd(mbed3)/sqrt(length(mbed3))


# approximate node ordering using chow liu's mwst algorithm
# test on 50 5 2 1 1000
setwd("50_5_2_1_5000/")
dts = list.files("data_rds/")
folder = "node_ordering_mmhc_bnlearn"
dir.create(folder, showWarnings = F)
dags = list.files("dag")
m = c()
for (i in 1:length(dts)) {
  data = readRDS(paste0("data_rds/", dts[i]))
  # x = bnlearn::chow.liu(data)
  j = ceiling(i / 10)
  dag = readRDS(paste0("dag/", dags[j]))
  G = dag2matrix(moral(dag))
  # root = bnlearn::node.ordering(dag)[1]
  # x = directing_tree(x, root)
  x = bnlearn::mmhc(data)
  g = dag2matrix(moral(x))
  res = edit_dist_graph(g, G)
  m = c(m, unlist(res))
  # ord = bnlearn::node.ordering(x)
  # saveRDS(ord, paste0(folder, "/", dts[i]))
}

mm = matrix(m, nrow = 100, byrow = T)
colMeans(mm)

# test accuracy 
mbed = pre = rec = c()
mbed1 = pre1 = rec1 = c()
mbed2 = pre2 = rec2 = c()
mbed3 = pre3 = rec3 = c()
dags = list.files("dag/", pattern = ".rds")
orders = list.files(folder)
for (i in 1:length(dags)) {
  name = strsplit(dags[i], ".rds")[[1]]
  files = list.files(paste0("mb_mmlcpt/"), pattern = name)
  dag = readRDS(paste0("dag/", dags[i]))
  vars = bnlearn::nodes(dag)
  mr = dag2matrix(bnlearn::moral(dag))
  for (j in 1:length(files)) {
    ord = readRDS(paste0(folder, "/", files[j]))
    ord = rev(ord)
    mbcpt = readRDS(paste0("mb_mmlcpt/", files[j]))
    mbcpt_sym = symmetry_correction(vars, mbcpt, "union")
    G = mblist2moral(mbcpt_sym, vars)
    # enforce morality
    G1 = wrsgraph::min_deg_moralization(G)
    G2 = wrsgraph::min_deficiency_moralization(G)
    G3 = wrsgraph::fixed_ordering_moralization(G, ord)
    # G4 = wrsgraph::fixed_ordering_moralization(G, orderingList[[k]])
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
    
  }
}

write.table(paste0(nvars, "_", maxNPas, "_2_1_", n), "../results.txt", append = T, row.names = F, col.names = F)
x = round(c(mean(mbed), 1.96*sd(mbed)/sqrt(length(mbed)), mean(pre), mean(rec)), 2)
write.table(matrix(x, nrow = 1), "../results.txt", append = T, row.names = F, col.names = F)

x1 = round(c(mean(mbed1), 1.96*sd(mbed1)/sqrt(length(mbed1)), mean(pre1), mean(rec1)), 2)
write.table(matrix(x1, nrow = 1), "../results.txt", append = T, row.names = F, col.names = F)

x2 = round(c(mean(mbed2), 1.96*sd(mbed2)/sqrt(length(mbed2)), mean(pre2), mean(rec2)), 2)
write.table(matrix(x2, nrow = 1), "../results.txt", append = T, row.names = F, col.names = F)

x3 = round(c(mean(mbed3), 1.96*sd(mbed3)/sqrt(length(mbed3)), mean(pre3), mean(rec3)), 2)
write.table(matrix(x3, nrow = 1), "../results.txt", append = T, row.names = F, col.names = F)
write.table(" ", "../results.txt", append = T, row.names = F, col.names = F)


