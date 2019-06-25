setwd("~/Documents/Experiments/mb_discovery_IJAR/80_5_4_1/")
nvars = 80
name = paste0(nvars, "_5_4_1_")
# for (itr in 1:5) {
#
#   sd = randSeed()
#   set.seed(sd)
#   dag = randDag(nvars, 5)
#   cpts = randCPTs(dag, 4, 1)
#   nodes = bnlearn::nodes(dag)
#   mbtrue = list()
#   i = 1
#   for (x in nodes) {
#     mbtrue[[i]] = bnlearn::mb(dag, x)
#     i = i + 1
#   }
#   saveRDS(dag, paste0("dag/", name, sd, ".rds"))
#   saveRDS(cpts, paste0("cpts/", name, sd, ".rds"))
#   saveRDS(mbtrue, paste0("mb_true/", name, sd, ".rds"))
#
#   for (n in c(100, 500, 2000, 5000)) {
#     for (j in 1:5) {
#       datasd = randSeed()
#       set.seed(datasd)
#       data = rbn(cpts, n)
#       saveRDS(data, paste0("data_rds/", n, "/", name, n, "_", sd, "_", datasd, ".rds"))
#       write.csv(data, paste0("data_csv/", n, "/", name, n, "_", sd, "_", datasd, ".csv"), row.names = F)
#       dt = factor2numeric(data)
#       write.table(dt, paste0("data_sll/", n, "/", name, n, "_", sd, "_", datasd), row.names = F, col.names = F)
#     }
#   }
# }# end itr




# sll
#setwd("~/Documents/Experiments/SLL-1.0/")
for (n in c(100, 500, 2000, 5000)) {

  datasets = list.files(paste0("data_sll/", n))
  for (ii in 1:length(datasets)) {
    system(paste0("~/Documents/Experiments/SLL-1.0/sll data_sll/", n, "/", datasets[ii], " -a sll-mb -t all --output-mb-file sll/", datasets[ii]))
  }

}

# pcmb, iamb



filename = strsplit(datasets[ii], ".csv")[[1]]
#filename = paste0(c("sll", strsplit(filename, "_")[[1]][-1]), collapse = "_")
saveRDS(mbl_sll, paste0("../", model, "/sll/", filename, ".rds")) # save learned mb as .rds
#file.remove("output.txt")


# evaluation
model = paste0(nvars, "_5_4_1")
true = list.files(paste0("mb_true"))
dags = list.files(paste0("dag"))
#dag = readRDS(paste0(dir, model, "/dag/", dags[1]))

m = matrix(0, 4, 15)
m[, 1] = model
amb = c() # average mb size
for (x in true) {

  mbt = readRDS(paste0("mb_true/", x))
  amb = c(amb, round(sum(sapply(mbt, length)) / length(mbt), 1))

}
m[, 2] = mean(amb)
m[, 3] = c(100, 500, 2000, 5000)
l = 4
#, "iamb_cpp", "iamb_bnlearn", "sll"
for (folder in c("mml_cpt", "mml_nb", "iamb_bnlearn_0.01", "pcmb_0.01", "sll")) {
  # , 500, 2000, 5000
  for (n in c(100, 500)) {

    if (n == 2000) {
      ind = 3
    } else if (n == 5000) {
      ind = 4
    } else if (n == 500) {
      ind = 2
    } else if (n == 100) {
      ind = 1
    }

    learned = list.files(paste0(folder), pattern = paste0("_", n, "_"))
    #datasets = list.files(paste0(dir, model, "/data_rds"), pattern = paste0("_", n, "_"))
    edList = rep(list(rep(0, length(mbt))), length(learned)) # edit distance
    retList = rep(list(c()), length(learned)) # precision and recall
    for (i in 1:length(true)) {
      dag = readRDS(paste0("dag/", dags[i]))
      vars = bnlearn::nodes(dag)
      mbt = readRDS(paste0("mb_true/", true[i]))
      for (j in ((5 * (i - 1)) + 1):(5 * i)) {

        #data = readRDS(paste0(dir, model, "/data_rds/", datasets[j]))
        if (folder == "sll") {
          mbl = sll2list(paste0(folder, "/", learned[j]), vars)
        } else {
          mbl = readRDS(paste0(folder, "/", learned[j]))
        }


        if (folder %in% c("mml_cpt", "mml_rand")) {
          mbl = symmetry_correction(vars, mbl, "union")
        } else if (folder %in% c("mml_nb")) {
          mbl = symmetry_correction(vars, mbl, "intersection")
        }

        for (k in 1:length(mbt)) {
          edList[[j]][k] = mb_false_finding(mbt[[k]], mbl[[k]])
          #cat(edList[[j]][k], ":")
          retList[[j]] = c(retList[[j]], round(mb_retrieval(mbt[[k]], mbl[[k]], length(mbt)), 1))
          #cat(round(mb_retrieval(mbt[[k]], mbl[[k]], length(mbt)), 1), "\n")
        }
      }# end for j

    }# end for i

    avg_ed_over_all_nodes = paste0(round(conf_int(unlist(edList)), 1), collapse = "+-")
    retVec = unlist(retList)
    avg_pre_over_all_nodes = paste0(round(conf_int(retVec[!gtools::even(1:length(retVec))]), 2), collapse = "+-")
    avg_rec_over_all_nodes = paste0(round(conf_int(retVec[gtools::even(1:length(retVec))]), 2), collapse = "+-")
    avg_pre_rec_over_all_nodes = paste(avg_pre_over_all_nodes, avg_rec_over_all_nodes)
    m[ind, l] = avg_ed_over_all_nodes
    m[ind, l + 6] = avg_pre_rec_over_all_nodes
  }# end for n
  l = l + 1
}# end for each method


m





