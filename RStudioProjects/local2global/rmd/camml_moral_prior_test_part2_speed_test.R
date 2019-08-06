# test camml's ability on taking moral prior
# 5 random BNs w/ 50 5 2 1
# each generates 5 datasets w/ 300 samples
# confidence levels are 0.5, 0.7, 0.9, 1
# moral prior correctness are measured by edit dist 0, 10, 30, 50, 70, 90
# the 5 BNs used have average # edges 121.6, degree 4.86, mb size 8.71
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
library(plotly)

nvars = 100
maxNPas = 5
n = 1000
name = paste0(nvars, "_", maxNPas, "_2_1_", n)

# generate random models and data
# generate random artificial priors w/ fixed ed to the true moral graph
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
    write.csv(data, paste0("data_csv/", name, "_", sd1, "_", sd2, ".csv"), row.names = F)
  }
  # dag_true = readRDS(paste0("dag/", dags[i]))
  # name = strsplit(dags[i], ".rds")[[1]]
  mr = bnlearn::moral(dag_true)
  mr = dag2matrix(mr)
  vars = colnames(mr)
  # random remove edges from moral graph
  for (k in c(60)) {
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
    for (p in c(0.5)) {
      text = "arcs {"
      for (row_i in 1:(nrow(mr)-1)) {
        for (col_j in (row_i+1):ncol(mr)) {
          if (mr[row_i,col_j]==1) text = paste(text, "\n", vars[row_i], "--", vars[col_j], p, ";")
        } # end for col_j
      }
      text = paste(text, "\n }")
      write_file(text, paste0("moral_prior/", k, "/", p, "/", name, "_", sd1, "_", p, ".txt"))
    }

  }

}

# calculate # of arcs in each true dag
dags = list.files("dag/")
edges = c()
for (i in 1:length(dags)) {
  dag = readRDS(paste0("dag/", dags[i]))
  edges = c(edges, nrow(dag$arcs))
}
edges
# according to camml, the default arc prior is (# edges + 0.5)/(k*(k-1)/2 + 1)
arcPriors = (edges + 0.5) / (100 * 99 / 2 + 1)
round(mean(arcPriors), 2) # approx 0.09

# learn mb using mmlcpt
dts = list.files("data_rds/")
for (j in 6:length(dts)) {
  data = readRDS(paste0("data_rds/", dts[j]))
  # learn mb using mmlcpt
  vars = colnames(data)
  # data_cat = numeric2categorical(data)
  arities = sapply(data, nlevels)
  di = count_occurance(data, arities)
  registerDoParallel(3)
  mbcpt = foreach(target = vars,
                  .combine = list,
                  .multicombine = TRUE) %dopar% {
                    forward_greedy_fast(data, di, arities, vars, n, target)
                  }
  stopImplicitCluster()
  names(mbcpt) = vars
  saveRDS(mbcpt, paste0("mb_mmlcpt/", dts[j]))
}

# calculate mmlcpt moral graph edit dist
# for 50 5 2 1 300
# average is 163.7+-2.2, pre=0.68, rec=0.46
# min deg moral 198+-3.1, pre=0.55, rec=0.54
# min deficiency moral 191.1+-2.9, pre=0.56, rec=0.54
# rev true node order 190+-4.3, pre=0.55, rec=0.69
# for 100 5 2 1 1000
# average is 276.5+-5.5, pre=0.87, rec=0.53
# min deg moral 324.9+-7.4, pre=0.70, rec=0.61
# min deficiency moral 302+-7.2, pre=0.74, rec=0.62
# rev true node order 215.1+-8.4, pre=0.79, rec=0.79
mbed = c()
pre = rec = c()
dags = list.files("dag/", pattern = ".rds")
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
    # G = wrsgraph::min_deg_moralization(G)
    # G = wrsgraph::min_deficiency_moralization(G)
    # G = wrsgraph::fixed_ordering_moralization(G, ord)
    res = edit_dist_graph(G, mr)
    mbed = c(mbed, res$ed)
    pre = c(pre, res$pre)
    rec = c(rec, res$rec)
    
  }
}
mean(mbed)
1.96*sd(mbed)/sqrt(length(mbed))
mean(pre)
mean(rec)

# learn mb using 

# learn mb using sll 
files = list.files("data_sll/")
for (i in 1:5) {
  system(paste0("./sll data_sll/", files[i], " -a sll-mb --output-mb-file mb_sll/", files[i]))
}

# evaluate sll on mb discovery 
mbed = c()
pre = rec = c()
dags = list.files("dag/", pattern = ".rds")
for (i in 1:length(dags)) {
  name = strsplit(dags[i], ".rds")[[1]]
  files = list.files("mb_sll", pattern = name)
  dag = readRDS(paste0("dag/", dags[i]))
  vars = bnlearn::nodes(dag)
  mr = dag2matrix(bnlearn::moral(dag))
  for (j in 1:length(files)) {
    mbsll = sll2list(paste0("mb_sll/", files[j]), vars)
    G = mblist2moral(mbsll, vars)
    res = edit_dist_graph(G, mr)
    mbed = c(mbed, res$ed)
    pre = c(pre, res$pre)
    rec = c(rec, res$rec)
    
  }
}
mean(mbed)
1.96*sd(mbed)/sqrt(length(mbed))
mean(pre)
mean(rec)

# generate mmlcpt mb prior w/ fixed conf
dags = list.files("dag/")
mbs = list.files("mb_mmlcpt/", ".rds")
for (k in 1:length(mbs)) {
  dag = readRDS(paste0("dag/", dags[ceiling(k/10)]))
  ord = rev(bnlearn::node.ordering(dag))
  name = strsplit(mbs[k], ".rds")[[1]]
  mbcpt = readRDS(paste0("mb_mmlcpt/", mbs[k]))
  vars = names(mbcpt)
  mbcpt_sym = symmetry_correction(vars, mbcpt, "union")
  mr = mblist2moral(mbcpt_sym, vars)
  # moralize 
  mr = wrsgraph::fixed_ordering_moralization(mr, ord)
  
  # make prior in txt file 
  for (p in c(0.3, 0.5)) {
    text = "arcs {"
    for (row_i in 1:(nrow(mr)-1)) {
      for (col_j in (row_i+1):ncol(mr)) {
        if (mr[row_i,col_j]==1) text = paste(text, "\n", vars[row_i], "--", vars[col_j], p, ";")
      } # end for col_j
    }
    text = paste(text, "\n }")
    write_file(text, paste0("moral_prior/mmlcpt_moral_fix_order/", p, "/", name, "_", p, ".txt"))
  }
}

# generate prior w/ dynamic confidence
mbs = list.files("mb_mmlcpt/", ".rds")
for (k in 1:length(mbs)) {
  name = strsplit(mbs[k], ".rds")[[1]]
  mbcpt = readRDS(paste0("mb_mmlcpt/", mbs[k]))
  vars = names(mbcpt)
  mbcpt_sym = symmetry_correction(vars, mbcpt, "union")
  mr = mblist2moral(mbcpt_sym)

  text = "arcs {"
  for (row_i in 1:(nrow(mr)-1)) {
    for (col_j in (row_i+1):ncol(mr)) {
      if (mr[row_i,col_j]==1) {
        x = vars[row_i]
        y = vars[col_j]
        if ((length(mbcpt[[x]]) > 0) && (length(mbcpt[[y]]) > 0) && (x == mbcpt[[y]][1]) && (y == mbcpt[[x]][1])) {
          text = paste(text, "\n", x, "--", y, "0.95;")
        } else {
          text = paste(text, "\n", x, "--", y, "0.5;")
        }
      }
    } # end for col_j
  }
  text = paste(text, "\n }")
  write_file(text, paste0("moral_prior/mmlcpt/0.95_0.5/", name, "_0.95_0.5.txt"))
}

########################## speed test with less iterations #####################
################################################################################
dags = list.files("dag/", ".rds")
# tempDir = "dag_camml/30/0.5/"
tempDir = "dag_camml/mmlcpt/0.3/"
# tempDir = "dag_others/camml/no/"
m = c()
for (i in 1:length(dags)) {
  name = strsplit(dags[i], ".rds")[[1]]
  files = list.files(tempDir, pattern = name)
  dag = readRDS(paste0("dag/", dags[i]))
  # m = matrix(0, ncol = length(dags), nrow = length(file s))
  for (j in 1:length(files)) {
    dne = readr::read_file(paste0(tempDir, files[j]))
    dag_camml = dne2bnlearn(dne)
    m = c(m, bnlearn::shd(dag_camml, dag))
  }
}
cat(mean(m), "\n")
cat(1.96*sd(m)/sqrt(length(m)), "\n")


################################################################################
################################################################################


################################ plots ######################################
################################################################################
library(ggplot2)
df = read.csv("results.csv")
error = read.csv("results_ci.csv")
error = error$ci
figure = ggplot(df, aes(x = metropolis, y = ed, group = prior, colour = prior, linetype = prior)) +
  ylab(label = "Edit distance") + xlab("Metropolis") + geom_line(aes(linetype = prior)) + 
  geom_point(aes(shape = prior)) + 
  scale_colour_manual(values = c("#E69F00", "#56B4E9", "#009E73", "#0072B2")) +
  ylim(floor(min(df$ed - error)), ceiling(max(df$ed + error))) + 
  scale_y_continuous(breaks = seq(floor(min(df$ed - error)), ceiling(max(df$ed + error)), by =5)) + 
  guides(linetype = guide_legend()) +
  theme(legend.key.width = unit(1.5, "cm")) +
  facet_wrap(~ annealing) + 
  geom_errorbar(aes(ymin = ed - error, ymax = ed + error), width = 0.03)
figure


################################################################################
################################################################################