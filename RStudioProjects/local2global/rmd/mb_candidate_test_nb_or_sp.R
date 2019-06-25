# estimate mb candidate confidence
# first, we look at how confidence that the 1st found variable is in the mb
# and how confidence that it is a direct nbr of the target in the dag
# setwd("~/Documents/Experiments/camml_moral_prior_test_20_may_2019/")
# library(bnlearn)
# library(wrsgraph)
# library(igraph)
# library(lglbnlearn)
# library(gtools)
# library(doParallel)
# library(readr)
# library(ggplot2)
# library(reshape)

# 1st: 0.847 in nbr, 0.851 in mb
# 2nd: 0.684 in nbr, 0.796 in mb
# 3rd: 0.588 in nbr, 0.761 in mb
# 4th: 0.489, 0.662
# 5th: 0.390, 0.550
# 6th: 0.316, 0.476
# 7th: 0.312, 0.387
# 8th: 0.143, 0.238
# 9th: 0.188, 0.188
# 10th: 0.167, 0.167

ndags = 50
nvars = 50
maxnpas = 5
maxarity = 2
n = 300
dags = datasets = list()
npara = c()
for (i in 1:ndags) {
  dag = randDag(nvars, maxnpas)
  cpts = randCPTs(dag, maxarity, 1)
  data = rbn(cpts, n)
  
  dags[[i]] = dag
  datasets[[i]] = data
  v = sapply(sapply(nodes(dag),bnlearn::parents, x = dag),length)
  npara[i] = sum(2 ^ v)
}



mbs = list()
ed = nmr = c()
for (i in 1:ndags) {
  if (i %% 5 == 0) cat(i, "\n")
  dag = dags[[i]]
  vars = bnlearn::nodes(dag)
  
  data_cat = numeric2categorical(datasets[[i]])
  arities = sapply(data_cat, nlevels)
  di = count_occurance(data_cat, arities)
  registerDoParallel(4) # use 3 cores
  mbcpt = foreach(target = vars,
                  .combine = list,
                  .multicombine = TRUE) %dopar% {
                    forward_greedy_fast(datasets[[i]], di, arities, vars, n, target)
                  }
  stopImplicitCluster()
  names(mbcpt) = vars
  mbs[[i]] = mbcpt
  
  # mbcpt = mbs[[i]]
  mr = dag2matrix(moral(dag))
  nmr[i] = sum(mr) / 2
  mbcpt = symmetry_correction(vars, mbcpt, "union")
  G = matrix(0, nvars, nvars)
  dimnames(G) = list(vars, vars)
  for (x in vars) {
    G[x, mbcpt[[x]]] = 1
  }
  ed[i] = sum(abs(mr - G)) / 2
}

is_nbr = is_mb = c()
for (ind in 1:10) {
  #dags = list.files("dag/", pattern = ".rds")
  isNbr = isMb = c()
  for (i in 1:length(mbs)) {
    
    dag = dags[[i]]
    # dag = readRDS(paste0("dag/", dags[i]))
    vars = bnlearn::nodes(dag)
    #for (j in 1:length(files)) {
    mbcpt = mbs[[i]]
    mbcpt = symmetry_correction(vars, mbcpt, "union")
    #mbcpt = symmetry_correction(vars, mbcpt, "intersection")
    
    for (k in 1:length(mbcpt)) {
      if (length(mbcpt[[k]]) >= ind) {
        isNbr = c(isNbr, mbcpt[[k]][ind] %in% bnlearn::nbr(dag, vars[k]))
        isMb = c(isMb, mbcpt[[k]][ind] %in% bnlearn::mb(dag, vars[k]))
      }
    }
    #}
  }
  is_nbr = c(is_nbr, sum(isNbr)/length(isNbr))
  is_mb = c(is_mb, sum(isMb)/length(isMb))
  #cat(sum(isNbr) / length(isNbr), "\n")
#cat(sum(isMb) / length(isMb), "\n")
}

cat(mean(ed), "\n")
cat(mean(nmr), "\n")
cat(mean(ed) / mean(nmr), "\n")
round(n/npara,1)
ord = order(n/npara,decreasing = T)
par(mfrow=c(1,2))
plot(npara[ord],ed[ord],type = "o")
# npara[ord]
# ed[ord]
plot(is_nbr, type = "o", col = "red", ylim = c(0,1))
lines(is_mb, type = "o", col="blue")
