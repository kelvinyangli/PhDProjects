# test camml's ability on taking moral prior
# 5 random BNs w/ 50 5 2 1
# each generates 5 datasets w/ 300 samples
# confidence levels are 0.5, 0.7, 0.9, 1
# moral prior correctness are measured by edit dist 0, 10, 20, 30, 40
#
setwd("~/Documents/Experiments/camml_moral_prior_test_20_may_2019/")
library(bnlearn)
library(wrsgraph)
library(igraph)
library(lglbnlearn)
library(gtools)
library(doParallel)
library(readr)
nvars = 50
maxNPas = 5
n = 300
name = paste0(nvars, "_", maxNPas, "_2_1_", n)
for (i in 1:5) {
  # sd1 = randSeed()
  # dag_true = randDag(nvars, maxNPas)
  # cpts = randCPTs(dag_true, 2, 1)
  # saveRDS(dag_true, paste0("dag/",name, "_", sd1, ".rds"))
  # saveRDS(cpts, paste0("cpts/",name, "_", sd1, ".rds"))
  # for (j in 1:5) {
  #   sd2 = randSeed()
  #   data = rbn(cpts, n)
  #   write.csv(data, paste0("data/", name, "_", sd1, "_", sd2, ".csv"), row.names = F)
  # }
  dag_true = readRDS(paste0("dag/", dags[i]))
  name = strsplit(dags[i], ".rds")[[1]]
  mr = bnlearn::moral(dag_true)
  mr = dag2matrix(mr)
  vars = colnames(mr)
  # random remove edges from moral graph
  for (k in c(50,70,90)) {
    if (k > 0) {
      # sample from indices of the upper triangular matrix
      indx = sample(which(as.vector(upper.tri(mr)) == TRUE), k, replace = F)
      # remove/add k random edges, depending on whether or not an edge in a
      # particular sampled entry exists
      m = as.vector(mr)
      for (ind in indx) {
        if (m[ind] == 0) {
          m[ind] = 1
        } else {
          m[ind] = 0
        }
      }
      mr = matrix(m, nrow = nvars, ncol = nvars, byrow = F)
      mr[lower.tri(mr)] = t(mr)[lower.tri(mr)]

    }

    # write into txt for camml prior
    for (p in c(0.5, 0.7, 0.9)) {
      text = "arcs {"
      for (i in 1:(nrow(mr)-1)) {
        for (j in (i+1):ncol(mr)) {
          if (mr[i,j]==1) text = paste(text, "\n", vars[i], "--", vars[j], p, ";")
        } # end for j
      }
      text = paste(text, "\n }")
      write_file(text, paste0("moral_prior/", k, "/", p, "/", name, "_", p, ".txt"))
    }

  }

}

# calculate mmlcpt mb ed 
mbed = matrix(0, ncol = 5, nrow = 5)
dags = list.files("dag/", pattern = ".rds")
for (i in 1:length(dags)) {
  name = strsplit(dags[i], ".rds")[[1]]
  files = list.files("data", pattern = name)
  dag = readRDS(paste0("dag/", dags[i]))
  mr = dag2matrix(bnlearn::moral(dag))
  for (j in 1:length(files)) {
    data = read.csv(paste0("data/", files[j]))
    # learn mb using mmlcpt
    vars = colnames(data)
    data_cat = numeric2categorical(data)
    arities = sapply(data_cat, nlevels)
    di = count_occurance(data_cat, arities)
    registerDoParallel(2) # use 3 cores
    mbcpt = foreach(target = vars,
                    .combine = list,
                    .multicombine = TRUE) %dopar% {
                      forward_greedy_fast(data, di, arities, vars, 300, target)
                    }
    stopImplicitCluster()
    names(mbcpt) = vars
    mbcpt_union = symmetry_correction(vars, mbcpt, "union")
    #mbcpt_inter = symmetry_correction(vars, mbcpt, "intersection")
    # form an undirected graph
    G = matrix(0, 50, 50)
    dimnames(G) = list(vars, vars)
    for (x in vars) {
      G[x, mbcpt_union[[x]]] = 1
    }
    mbed[j, i] = sum(abs(mr - G)) / 2
    #edit_dist_graph(G, mr)$ed
  }
}

mm = c()
# evaluate camml on no prior 
m = matrix(0, ncol = 5, nrow = 5)
#dags = list.files("dag/", pattern = ".rds")
for (i in 1:length(dags)) {
  name = strsplit(dags[i], ".rds")[[1]]
  files = list.files("dag_camml/no", pattern = name)
  dag = readRDS(paste0("dag/", dags[i]))
  for (j in 1:length(files)) {
    dne = readr::read_file(paste0("dag_camml/no/", files[j]))
    dag_camml = dne2bnlearn(dne)
    m[j, i] = bnlearn::shd(dag_camml, dag)
  }
}
mm = c(mm, colMeans(m))

# evaluate camml w/ moral prior w/ 0% incorrectness and 0.5 confidence 
for (ed in c(0, 10, 30)) {
  for (conf in c(0.9, 0.7, 0.5)) {
    m2 = matrix(0, 5, 5) 
    # ed = 0 
    # conf = 0.9
    for (i in 1:length(dags)) {
      name = strsplit(dags[i], ".rds")[[1]]
      tempDir = paste0("dag_camml/", ed, "/", conf, "/")
      files = list.files(tempDir, pattern = name)
      dag = readRDS(paste0("dag/", dags[i]))
      for (j in 1:length(files)) {
        dne = readr::read_file(paste0(tempDir, files[j]))
        dag_camml = dne2bnlearn(dne)
        m2[j, i] = bnlearn::shd(dag_camml, dag)
      }
    }
    mm = c(mm, colMeans(m2))
  }
}

res = matrix(mm, ncol = 5, byrow = T)
res
apply(res, 1, mean)



