#setwd("~/Documents/Experiments/SLL/")
# model specification
# test 2 results 30 5 2 1 8000 on ed to the true pattern: 
# greedy search w/ sll im skeleton 105.3
# greedy search w/ sll m skeleton 106
# greedy search w/ mml m skeleton 113.5
library(bnlearn)
library(gtools)
library(wrsgraph)
library(lglbnlearn)
library(doParallel)
library(igraph)
nvars = 30
maxNPas = 5 
maxArity = 2
beta = 1
n = 2000
nRep = 10
dir = "test11/"

for (i in 1:nRep) {
  # generate models
  seed = randSeed()
  dag = randDag(nvars, maxNPas)
  cpts = randCPTs(dag, maxArity, beta)
  data = rbn(cpts, n)
  data = factor2numeric(data)
  name = paste0(nvars, "_", maxNPas, "_", maxArity, "_", beta, "_", n, "_", seed)
  saveRDS(dag, paste0(dir, "dag_true/", name, ".rds"))
  #saveRDS(cpts, paste0(dir, "cpts_true/", name, ".rds"))
  write.table(data, paste0(dir, "dat/", name), row.names = F, col.names = F)
}
  
# learn bn using sll
dts = list.files(paste0(dir, "dat/"))
#nRep = length(dts)
for (i in 1:nRep) {
  system(paste0("./sll ", dir, "dat/", dts[i], 
                " -a sll+g --output-dag-file ", dir, "dag_sll/", dts[i], 
                " --output-skeleton-file ", dir, "skeleton_sll/", dts[i], 
                " --output-mb-file ", dir, "mb_sll/", dts[i]))
}

# convert sll mb list into adj mtx 
for (j in 1:nRep) {
  mb = read.table(paste0(dir, "mb_sll/", dts[j]), sep = "\n")
  mtx = matrix(0, nrow = nvars, ncol = nvars)
  for (i in 1:nvars) {
    temp = strsplit(as.character(mb[i, 1]), ": ")[[1]][-1]
    if (length(temp) > 0) {
      ind = as.numeric(strsplit(temp, " ")[[1]]) + 1
      mtx[i, ind] = 1
    } 
  }
  write.table(mtx, paste0(dir, "mb_sll/", dts[j]), row.names = F, col.names = F)
}

r = c()
# test skeleton morality
for (i in 1:nRep) {
  mb = read.table(paste0(dir, "mb_sll/", dts[i]))
  rownames(mb) = colnames(mb)
  mb = as.matrix(mb)
  moral = wrsgraph::wrs_bktr(mb, mb)$wrs
  r = c(r, moral)
  # dag_true = readRDS(paste0(dir, "dag_true/", dts[i], ".rds"))
  # ord = rev(node.ordering(dag_true))
  # if (moral == 0) mb = fixed_ordering_moralization(mb, ord)
  if (moral == 0) mb = min_deg_moralization(mb)
  write.table(mb, paste0(dir, "mb_sll_m/", dts[i]), row.names = F, col.names = F)
}
r

# learn w/ moral skeleton
#dts = list.files("~/Documents/Experiments/SLL/dat/")
for (i in 1:nRep) {
  system(paste0("./sll2 -i ", dir, "dat/", dts[i], " -s ", dir, "mb_sll_m/", dts[i]," -a sll+g --output-dag-file ", dir, "dag_sll_m/", dts[i]))
}


# learn mb using mml 
for (i in 1:nRep) {
  dt = read.table(paste0(dir, "dat/", dts[i]))
  #dt = dt + 1
  vars = colnames(dt)
  data_cat = numeric2categorical(dt)
  arities = sapply(data_cat, nlevels)
  di = varCnt = count_occurance(data_cat, arities)
  
  ## learn mbs using parallel mmlcpt 
  registerDoParallel(3) # use 3 cores 
  res = foreach(target = vars,
                .combine = list,
                .multicombine = TRUE) %dopar% {
                  forward_greedy_fast(dt, di, arities, vars, n, target)
                }
  stopImplicitCluster()
  mbcpt = res
  names(mbcpt) = names(res) = vars
  mbcpt = symmetry_correction(vars, mbcpt, "AND")
  
  # form an undirected graph
  G = matrix(0, nvars, nvars)
  dimnames(G) = list(vars, vars)
  for (x in vars) {
    G[x, mbcpt[[x]]] = 1
  }
  
  write.table(G, paste0(dir, "mb_mml/", dts[i]), row.names = F, col.names = F)

}

# enforce morality
r = c()
for (i in 1:nRep) {
  mb = read.table(paste0(dir, "mb_mml/", dts[i]))
  rownames(mb) = colnames(mb)
  mb = as.matrix(mb)
  moral = wrsgraph::wrs_bktr(mb, mb)$wrs
  r = c(r, moral)
  if (moral == 0) {
    # dag_true = readRDS(paste0(dir, "dag_true/", dts[i], ".rds"))
    # ord = rev(node.ordering(dag_true))
    # mb = fixed_ordering_moralization(mb, ord)
    mb = min_deg_moralization(mb)
  }
  write.table(mb, paste0(dir, "mb_mml_m/", dts[i]), row.names = F, col.names = F)
}
r


# learn w/ mml skeleton
#dts = list.files("~/Documents/Experiments/SLL/dat/")
for (i in 1:nRep) {
  system(paste0("./sll2 -i ", dir, "dat/", dts[i], " -s ", dir, "mb_mml/", dts[i]," -a sll+g --output-dag-file ", dir, "dag_mml/", dts[i]))
  system(paste0("./sll2 -i ", dir, "dat/", dts[i], " -s ", dir, "mb_mml_m/", dts[i]," -a sll+g --output-dag-file ", dir, "dag_mml_m/", dts[i]))
}


# orient moral skeleton to a dag 
#sks = list.files("skeleton_moral_mml/")
# dag_mml_arb_by_im = rep(0, nRep)
# for (i in 1:nRep) {
#   dag_true = readRDS(paste0("dag_true/", dts[i], ".rds"))
#   sk = read.table(paste0("skeleton_mml_m/", dts[i]))
#   rownames(sk) = colnames(sk)
#   sk = as.matrix(sk)
#   g = sk
#   cat(i)
#   dag_arbitrary = wrs_bktr(sk, g)$dag
#   dag_arbitrary = matrix2dag(dag_arbitrary)
#   dag_mml_arb_by_im[i] = shd(dag_arbitrary, dag_true)
# }



dir = "test11/"
dts = list.files(paste0(dir, "dat"))
name = strsplit(dts[1],"_")[[1]][1:5]
name = paste0(c(name, length(dts)), collapse = " ")
# cal the number of edges 
# r = s = u = v = x = y = c()
# for (i in 1:length(dts)) {
#   mb_sll = read.table(paste0(dir, "mb_sll/", dts[i]))
#   sk_sll = read.table(paste0(dir, "skeleton_sll/", dts[i]))
#   dag_sll = read.table(paste0(dir, "dag_sll/", dts[i]))
#   r = c(r, sum(sk_sll)/2)
#   u = c(u, sum(dag_sll)/2)
#   sk_mml = read.table(paste0(dir, "mb_mml/", dts[i]))
#   dag_mml = read.table(paste0(dir, "dag_mml/", dts[i]))
#   s = c(s, sum(sk_mml)/2)
#   v = c(v, sum(dag_mml)/2)
# }
# 
# mean(r) # sk sll
# mean(s) # sk mml
# mean(u) # dag sll
# mean(v) # dag mml

# test ed distance 
dag_sll_im = dag_sll_m = rep(0, length(dts))
for (i in 1:length(dts)) {
  dag_true = readRDS(paste0(dir, "dag_true/", dts[i], ".rds"))
  dag_im = read.table(paste0(dir, "dag_sll/", dts[i]))
  dag_m = read.table(paste0(dir, "dag_sll_m/", dts[i]))
  row.names(dag_im) = colnames(dag_im)
  row.names(dag_m) = colnames(dag_m)
  dag_im = matrix2dag(dag_im)
  dag_m = matrix2dag(dag_m)
  dag_sll_im[i] = shd(dag_im, dag_true)
  dag_sll_m[i] = shd(dag_m, dag_true)  
}

# orient moral skeleton to a dag 
dag_sll_arb_by_im = rep(0, length(dts))
# for (i in 1:length(dts)) {
#   dag_true = readRDS(paste0(dir, "dag_true/", dts[i], ".rds"))
#   sk = read.table(paste0(dir, "mb_sll_m/", dts[i]))
#   rownames(sk) = colnames(sk)
#   sk = as.matrix(sk)
#   g = sk
#   cat(i)
#   dag_arbitrary = wrs_bktr(sk, g)$dag
#   dag_arbitrary = matrix2dag(dag_arbitrary)
#   dag_sll_arb_by_im[i] = shd(dag_arbitrary, dag_true)
# }

# test ed distance of mml results
dag_mml_m = dag_mml_im = rep(0, length(dts))
mb_mml_list = mb_mml_m_list = mb_sll_list = mb_sll_m_list = rep(0, length(dts))
for (i in 1:length(dts)) {
  dag_true = readRDS(paste0(dir, "dag_true/", dts[i], ".rds"))
  dag_m_mml = read.table(paste0(dir, "dag_mml_m/", dts[i]))
  dag_im_mml = read.table(paste0(dir, "dag_mml/", dts[i]))
  row.names(dag_m_mml) = colnames(dag_m_mml)
  row.names(dag_im_mml) = colnames(dag_im_mml)
  dag_m_mml = matrix2dag(dag_m_mml)
  dag_im_mml = matrix2dag(dag_im_mml)
  dag_mml_m[i] = shd(dag_m_mml, dag_true)  
  dag_mml_im[i] = shd(dag_im_mml, dag_true)  
  
  moral_true = dag2matrix(bnlearn::moral(dag_true))
  #sk_true = dag2matrix(skeleton(dag_true))
  mb_sll = read.table(paste0(dir, "mb_sll/", dts[i]))
  mb_sll_m = read.table(paste0(dir, "mb_sll_m/", dts[i]))
  mb_sll_list[i] = sum(abs(moral_true - mb_sll)) / nvars
  mb_sll_m_list[i] = sum(abs(moral_true - mb_sll_m)) / nvars
  
  mb_mml = read.table(paste0(dir, "mb_mml/", dts[i]))
  mb_mml_m = read.table(paste0(dir, "mb_mml_m/", dts[i]))
  mb_mml_list[i] = sum(abs(moral_true - mb_mml)) / nvars
  mb_mml_m_list[i] = sum(abs(moral_true - mb_mml_m)) / nvars
}

# orient moral skeleton to a dag 
# dag_mml_arb_by_im = rep(0, length(dts))
# for (i in 1:length(dts)) {
#   dag_true = readRDS(paste0(dir, "dag_true/", dts[i], ".rds"))
#   sk = read.table(paste0(dir, "mb_mml_m/", dts[i]))
#   rownames(sk) = colnames(sk)
#   sk = as.matrix(sk)
#   g = sk
#   cat(i)
#   dag_arbitrary = wrs_bktr(sk, g)$dag
#   dag_arbitrary = matrix2dag(dag_arbitrary)
#   dag_mml_arb_by_im[i] = shd(dag_arbitrary, dag_true)
# }

# dag_sll_im
# dag_sll_m
# dag_sll_arb_by_im
# 
# dag_mml_im
# dag_mml_m
# dag_mml_arb_by_im

# mean(mb_sll)
# mean(mb_mml)
# mean(dag_sll_im)
# mean(dag_sll_m)
# mean(dag_mml_im)
# #mean(dag_mml_m)
# mean(dag_sll_arb_by_im)

write(paste("* ", dir, name, "----------------"), file = "exp_results", append = T)
# write(paste("sk sll:", mean(r)), file="exp_results",append=T)
# write(paste("sk mml:", mean(s)), file="exp_results",append=T)
# write(paste("dag sll:", mean(u)), file="exp_results",append=T)
# write(paste("dag mml:", mean(v)), file="exp_results",append=T)
write(paste("mb_sll:", round(mean(mb_sll_list),2)), file="exp_results",append=T)
write(paste("mb_sll_m:", round(mean(mb_sll_m_list),2)), file="exp_results",append=T)

write(paste("mb_mml:", round(mean(mb_mml_list),2)), file="exp_results",append=T)
write(paste("mb_mml_m:", round(mean(mb_mml_m_list),2)), file="exp_results",append=T)

write(paste("dag_sll_im:", mean(dag_sll_im)), file="exp_results",append=T)
write(paste("dag_sll_m:", mean(dag_sll_m)), file="exp_results",append=T)
write(paste("dag_sll_arb_by_im:", mean(dag_sll_arb_by_im)), file="exp_results",append=T)

write(paste("dag_mml_im:", mean(dag_mml_im)), file="exp_results",append=T)
write(paste("dag_mml_m:", mean(dag_mml_m)), file="exp_results",append=T)
# write(paste("dag_mml_arb_by_im:", mean(dag_mml_arb_by_im)), file="exp_results",append=T)




