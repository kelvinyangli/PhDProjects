# test camml's ability on taking moral prior
# 5 random BNs w/ 50 5 2 1
# each generates 5 datasets w/ 300 samples
# confidence levels are 0.5, 0.7, 0.9, 1
# moral prior correctness are measured by edit dist 0, 10, 30, 50, 70, 90
# the 5 BNs used have average # edges 121.6, degree 4.86, mb size 8.71
setwd("~/Documents/Experiments/camml_moral_prior_test_20_may_2019/")
library(bnlearn)
library(wrsgraph)
library(igraph)
library(lglbnlearn)
library(gtools)
library(doParallel)
library(readr)
library(ggplot2)
library(reshape)
nvars = 50
maxNPas = 5
n = 300
name = paste0(nvars, "_", maxNPas, "_2_1_", n)

# generate random models and data 
# generate random artificial priors w/ fixed ed to the true moral graph 
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

# learn mb using mmlcpt 
dts = list.files("data/")
for (j in 1:length(dts)) {
  data = read.csv(paste0("data/", dts[j]))
  # learn mb using mmlcpt
  vars = colnames(data)
  data_cat = numeric2categorical(data)
  arities = sapply(data_cat, nlevels)
  di = count_occurance(data_cat, arities)
  registerDoParallel(3) # use 3 cores
  mbcpt = foreach(target = vars,
                  .combine = list,
                  .multicombine = TRUE) %dopar% {
                    forward_greedy_fast(data, di, arities, vars, n, target)
                  }
  stopImplicitCluster()
  names(mbcpt) = vars
  saveRDS(mbcpt, paste0("mb_mmlcpt/", strsplit(dts[j], ".csv")[[1]], ".rds"))
}

# calculate mmlcpt moral graph edit dist 
# average is 162.36 w/ sd 15.9
mbed = matrix(0, ncol = 5, nrow = 5)
dags = list.files("dag/", pattern = ".rds")
for (i in 1:length(dags)) {
  name = strsplit(dags[i], ".rds")[[1]]
  files = list.files("mb_mmlcpt", pattern = name)
  dag = readRDS(paste0("dag/", dags[i]))
  vars = bnlearn::nodes(dag)
  mr = dag2matrix(bnlearn::moral(dag))
  for (j in 1:length(files)) {
    # data = read.csv(paste0("data/", files[j]))
    # learn mb using mmlcpt
    # vars = colnames(data)
    # data_cat = numeric2categorical(data)
    # arities = sapply(data_cat, nlevels)
    # di = count_occurance(data_cat, arities)
    # registerDoParallel(3) # use 3 cores
    # mbcpt = foreach(target = vars,
    #                 .combine = list,
    #                 .multicombine = TRUE) %dopar% {
    #                   forward_greedy_fast(data, di, arities, vars, 300, target)
    #                 }
    # stopImplicitCluster()
    # names(mbcpt) = vars
    # saveRDS(mbcpt, paste0("mb_mmlcpt/", strsplit(files[j], ".csv")[[1]], ".rds"))
    mbcpt = readRDS(paste0("mb_mmlcpt/", files[j]))
    mbcpt_sym = symmetry_correction(vars, mbcpt, "union")
    #mbcpt_sym = symmetry_correction(vars, mbcpt, "intersection")
    # form an undirected graph
    G = matrix(0, 50, 50)
    dimnames(G) = list(vars, vars)
    for (x in vars) {
      G[x, mbcpt_sym[[x]]] = 1
    }
    mbed[j, i] = sum(abs(mr - G)) / 2
    #edit_dist_graph(G, mr)$ed
  }
}
mean(mbed)


################################################################################

#                             Evaluation 
                             
################################################################################
# evaluate camml on no prior 
# average edit dist on BNs is 41.36 w/ sd 9.35
dags = list.files("dag/", ".rds")
mm = ss = c()
# m = matrix(0, ncol = 5, nrow = 5)
#l_no = list()
#k = 1
#dags = list.files("dag/", pattern = ".rds")
m = c()
for (i in 1:length(dags)) {
  name = strsplit(dags[i], ".rds")[[1]]
  files = list.files("dag_camml/no", pattern = name)
  dag = readRDS(paste0("dag/", dags[i]))
  # m = matrix(0, ncol = length(dags), nrow = length(files))
  for (j in 1:length(files)) {
    dne = readr::read_file(paste0("dag_camml/no/", files[j]))
    dag_camml = dne2bnlearn(dne)
    m = c(m, bnlearn::shd(dag_camml, dag))
    # m[j, i] = bnlearn::shd(dag_camml, dag)
    # l_no[[k]] = round(unlist(edit_dist_dag(dag2matrix(dag_camml), dag2matrix(dag))),1)
    # k = k + 1
  }
}
# mean(m)
# sd(m)
confs = c(0.9,0.7,0.5)
mm = c(mm, mean(m))
ss = c(ss, sd(m))
eds = c(0,10,30,50,70,90,160,170,180,190,200)
# evaluate camml w/ moral prior w/ 0% incorrectness and 0.5 confidence 
for (conf in confs) {
  for (ed in eds) {
    # m2 = matrix(0, 5, 5) 
    m2 = c()
    #ed = 160
    #conf = 0.7
    k = 1
    l_mb_0.7 = list()
    for (i in 1:length(dags)) {
      name = strsplit(dags[i], ".rds")[[1]]
      tempDir = paste0("dag_camml/", ed, "/", conf, "/")
      files = list.files(tempDir, pattern = name)
      dag = readRDS(paste0("dag/", dags[i]))
      for (j in 1:length(files)) {
        dne = readr::read_file(paste0(tempDir, files[j]))
        dag_camml = dne2bnlearn(dne)
        m2 = c(m2, bnlearn::shd(dag_camml, dag))
        # m2[j, i] = bnlearn::shd(dag_camml, dag)
        l_mb_0.7[[k]] = round(unlist(edit_dist_dag(dag2matrix(dag_camml), dag2matrix(dag))),1)
        k = k + 1
      }
    }
    cat(length(m2), " ")
    mm = c(mm, mean(m2))
    ss = c(ss, sd(m2))
  }
}

res_ss = matrix(ss[-1], ncol = length(confs), byrow = F)
res = matrix(mm[-1], ncol = length(confs), byrow = F)
res = cbind(eds,res)
colnames(res) = c("ed", "0.9", "0.7", "0.5")
# res
# 
# plot(res[1,], type="o",col="blue",ylim=c(min(res), max(res)))
# lines(res[2,], type = "o", col="red") # 0.7
# lines(res[3,], type = "o", col="green") # 0.5
# abline(h=c(mm[1]))
# axis(3, labels=c(0,10,30,50),at=c(0,10,30,50))

# df = cbind(value = mm[-1],ed = rep(c(0,10,30,50,70,90), 3), 
#            conf = c(rep(0.9, 6),rep(0.7, 6),rep(0.5, 6)))
# df = cbind(m0[,1], m3[,1], log(m0[,4]))
# colnames(df) = c("uniform", "true", "prior")
df = as.data.frame(res)
m_melt = melt(df, "ed")
error = 1.96 * ss[-1] / sqrt(25)
figure = ggplot(m_melt, aes(x = ed, y = value, group = variable, colour = variable))+
  ylab(label = "Edit distance (between learned and true DAG pattern)") + 
  xlab("Moral prior error (measured by edit distance)") + 
  geom_line(aes(linetype = variable)) + geom_point(aes(shape = variable))+
  guides(linetype = guide_legend()) + 
  geom_hline(yintercept = mm[1], linetype="dashed") + 
  geom_hline(yintercept = mm[1]+1.96 * ss[1] / sqrt(25), linetype="dashed", color = "orange") + 
  geom_hline(yintercept = mm[1]-1.96 * ss[1] / sqrt(25), linetype="dashed", color = "orange") + 
  scale_y_continuous(breaks=seq(0, 55, 5)) + 
  scale_x_continuous(breaks=seq(0, 200, 10)) + 
  geom_errorbar(aes(ymin = value - error, ymax = value + error), width = 0.03)
  #theme(legend.key.width = unit(1.5, "cm")) + ylim(0, 10) + xlim(-3, 5)
figure
# ggsave("camml_moral_prior_test.pdf", device = "pdf", width = 7.29, height = 4.5, units = "in")
################################################################################
################################################################################

# test sll's accuracy on these BNs
files = list.files("data_sll/")
for (i in 1:length(files)) {
  #dt = read.csv(paste0("data/", files[i]))
  #name = strsplit(files[i], ".csv")[[1]]
  #dt = factor2numeric(dt)
  #write.table(dt, paste0("data_sll/", name), row.names = F, col.names = F)
  system(paste0("./sll data_sll/", files[i],
                " -a sll-mb --output-mb-file mb_sll/", files[i]))
                #" -a sll-mb --output-dag-file dag_sll/", files[i],
                #" --output-neigh-file nbr_sll/", files[i],
                #" --output-mb-file mb_sll/", files[i]))

}

# camml using mmlcpt mb prior with dynamic arc confidence 
# [1] 0.8283881 0.6468189 0.5438298 0.4577068 0.3882091 0.4401773 0.4491018
# [8] 0.4664723 0.4763948 0.5533333
# use 0.96 for the 1st mb candidate y of x s.t. x is also the 1st mb candidate
# of y, because we are highly confident that they are connected in dag
# estimate mb candidate being TP nbr 
# mbs = list.files("mb_mmlcpt/")
# dags = list.files("dag/", ".rds")
# is_nbr = is_mb = is_double_nbr = is_double_mb = c()
# for (ind in 1:10) {
#   #dags = list.files("dag/", pattern = ".rds")
#   isNbr = isMb = isDoubleNbr = isDoubleMb = c()
#   for (i in 1:length(dags)) {
# 
#     dag = readRDS(paste0("dag/", dags[i]))
#     vars = bnlearn::nodes(dag)
#     name = strsplit(dags[i], ".rds")[[1]]
#     files = list.files("mb_mmlcpt/", name)
#     for (j in 1:length(files)) {
#       mbcpt = readRDS(paste0("mb_mmlcpt/", files[j]))
#       mbcpt = symmetry_correction(vars, mbcpt, "union")
#       #mbcpt = symmetry_correction(vars, mbcpt, "intersection")
# 
#       for (k in 1:length(mbcpt)) {
#         if (length(mbcpt[[k]]) >= ind) {
#           isNbr = c(isNbr, mbcpt[[k]][ind] %in% bnlearn::nbr(dag, vars[k]))
#           isMb = c(isMb, mbcpt[[k]][ind] %in% bnlearn::mb(dag, vars[k]))
#           if (ind == 1) {
#             x = vars[k]
#             if (length(mbcpt[[k]]) > 0) {
#               y = mbcpt[[k]][1]
#               if ((length(mbcpt[[y]]) > 0) && (x == mbcpt[[y]][1])) {
#                 isDoubleNbr = c(isDoubleNbr, y %in% bnlearn::nbr(dag, x))
#                 isDoubleMb = c(isDoubleMb, y %in% bnlearn::mb(dag, x))
#               }
#             }
#             
#           }
#         }
#       }
#     }
#   }
#   cat(length(isDoubleMb), "\n")
#   is_nbr = c(is_nbr, sum(isNbr)/length(isNbr))
#   is_mb = c(is_mb, sum(isMb)/length(isMb))
#   is_double_mb = c(is_double_mb, sum(isDoubleMb)/length(isDoubleMb))
#   is_double_nbr = c(is_double_nbr, sum(isDoubleMb)/length(isDoubleMb))
#   #cat(sum(isNbr) / length(isNbr), "\n")
#   #cat(sum(isMb) / length(isMb), "\n")
# }


mbs = list.files("mb_mmlcpt/", ".rds")
# generate prior w/ dynamic confidence 
for (k in 1:length(mbs)) {
  name = strsplit(mbs[k], ".rds")[[1]]
  mbcpt = readRDS(paste0("mb_mmlcpt/", mbs[k]))
  vars = names(mbcpt)
  mbcpt_sym = symmetry_correction(vars, mbcpt, "union")
  #mbcpt_sym = symmetry_correction(vars, mbcpt, "intersection")
  # form an undirected graph
  mr = mblist2moral(mbcpt_sym)
  
  #for (p in c(0.5, 0.7)) {
    text = "arcs {"
    for (i in 1:(nrow(mr)-1)) {
      for (j in (i+1):ncol(mr)) {
        if (mr[i,j]==1) {
          x = vars[i]
          y = vars[j]
          if ((length(mbcpt[[x]]) > 0) && (length(mbcpt[[y]]) > 0) && (x == mbcpt[[y]][1]) && (y == mbcpt[[x]][1])) {
            text = paste(text, "\n", x, "--", y, "0.95;")  
          } else {
            text = paste(text, "\n", x, "--", y, "0.3;")
          }
        }
      } # end for j
    }
    text = paste(text, "\n }")
    write_file(text, paste0("moral_prior/mmlcpt/dynamic2/", name, "_dynamic2.txt"))
  #}
}

# generate prior w/ fixed confidence using mmlcpt
mbs = list.files("mb_mmlcpt/", ".rds")
for (k in 1:length(mbs)) {
  name = strsplit(mbs[k], ".rds")[[1]]
  mbcpt = readRDS(paste0("mb_mmlcpt/", mbs[k]))
  vars = names(mbcpt)
  mbcpt_sym = symmetry_correction(vars, mbcpt, "union")
  #mbcpt_sym = symmetry_correction(vars, mbcpt, "intersection")
  # form an undirected graph
  mr = mblist2moral(mbcpt_sym)
  
  for (p in c(0.3)) {
    text = "arcs {"
    for (i in 1:(nrow(mr)-1)) {
      for (j in (i+1):ncol(mr)) {
        if (mr[i,j]==1) text = paste(text, "\n", vars[i], "--", vars[j], p, ";")
      } # end for j
    }
    text = paste(text, "\n }")
    write_file(text, paste0("moral_prior/mmlcpt/", p, "/", name, "_", p, ".txt"))
  }
}

# evaluate sll 
# average edit dist on BN is 104.36 w/ sd 7.39
m = matrix(0, ncol = 5, nrow = 5)
dags = list.files("dag/", pattern = ".rds")
for (i in 1:length(dags)) {
  name = strsplit(dags[i], ".rds")[[1]]
  files = list.files("dag_sll/", pattern = name)
  dag = readRDS(paste0("dag/", dags[i]))
  for (j in 1:length(files)) {
    dag_sll = read.table(paste0("dag_sll/", files[j]))
    colnames(dag_sll) = rownames(dag_sll) = bnlearn::nodes(dag)
    dag_sll = matrix2dag(dag_sll)
    m[j, i] = bnlearn::shd(dag_sll, dag)
  }
}
mean(m)

# calculate sll moral graph edit dist 
# average is 168.92 w/ sd 14.07
mbed = matrix(0, ncol = 5, nrow = 5)
dags = list.files("dag/", pattern = ".rds")
for (k in 1:length(dags)) {
  name = strsplit(dags[k], ".rds")[[1]]
  files = list.files("mb_sll/", pattern = name)
  dag = readRDS(paste0("dag/", dags[k]))
  vars = bnlearn::nodes(dag)
  mr = dag2matrix(bnlearn::moral(dag))
  for (j in 1:length(files)) {
    mb = read.table(paste0("mb_sll/", files[j]), sep = "\n")
    mtx = matrix(0, nrow = 50, ncol = 50)
    for (i in 1:50) {
      temp = strsplit(as.character(mb[i, 1]), ": ")[[1]][-1]
      if (length(temp) > 0) {
        ind = as.numeric(strsplit(temp, " ")[[1]]) + 1
        mtx[i, ind] = 1
      } 
    }
    dimnames(mtx) = list(vars, vars)
    mbed[j, k] = sum(abs(mr - mtx)) / 2
  }
}
mean(mbed)









