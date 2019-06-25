setwd("~/Documents/Experiments/mb_discovery_IJAR/")
model = "80_5_4_1"
true = list.files(paste0(model, "/mb_true"))
dags = list.files(paste0(model, "/dag"))
#dag = readRDS(paste0(dir, model, "/dag/", dags[1]))

folders = c("mml_cpt", "mml_nb", "iamb_bnlearn_0.01", "pcmb_0.01", "sll")
ss = c(100, 500, 2000, 5000)
m = matrix(0, length(ss), 3+2*length(folders))
m[, 1] = model
amb = c() # average mb size
for (x in true) {

  mbt = readRDS(paste0(model, "/mb_true/", x))
  amb = c(amb, round(sum(sapply(mbt, length)) / length(mbt), 1))

}
m[, 2] = mean(amb)
m[, 3] = ss
l = 4


for (folder in folders) {

  for (n in ss) {

    if (n == 2000) {
      ind = 3
    } else if (n == 5000) {
      ind = 4
    } else if (n == 500) {
      ind = 2
    } else if (n == 100) {
      ind = 1
    }

    learned = list.files(paste0(model, "/", folder), pattern = paste0("_", n, "_"))
    datasets = list.files(paste0(model, "/data_rds/", n), pattern = paste0("_", n, "_"))
    edList = rep(list(rep(0, length(mbt))), length(learned)) # edit distance
    retList = rep(list(c()), length(learned)) # precision and recall
    for (i in 1:length(true)) {
      dag = readRDS(paste0(model, "/dag/", dags[i]))
      vars = bnlearn::nodes(dag)
      mbt = readRDS(paste0(model, "/mb_true/", true[i]))
      for (j in ((5 * (i - 1)) + 1):(5 * i)) {

        #data = readRDS(paste0(dir, model, "/data_rds/", datasets[j]))
        if (folder == "sll") {
          # mbl = readRDS(paste0(model, "/", folder, "/", learned[j]))
          mbl = sll2list(paste0(model, "/", folder, "/", learned[j]), vars)
        } else {
          mbl = readRDS(paste0(model, "/", folder, "/", learned[j]))
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
    m[ind, l + length(folders)] = avg_pre_rec_over_all_nodes
  }# end for n
  l = l + 1
}# end for each method


colnames(m) = c("model", "avg_mb", "n", rep(folders, 2))
m
