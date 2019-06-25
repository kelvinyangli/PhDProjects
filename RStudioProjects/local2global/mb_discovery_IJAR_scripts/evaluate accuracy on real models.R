setwd("~/Documents/Experiments/mb_discovery_IJAR/")
for (model in c("insurance")) {

  dag = readRDS(paste0(model, "/dag/", model, ".rds"))
  vars = bnlearn::nodes(dag)
  nvars = length(vars)
  mbt = lapply(vars, bnlearn::mb, x = dag)
  amb = mean(sapply(mbt, length))
  m = matrix(0, 3, 15)
  m[, 1] = model
  m[, 2] = round(amb, 1)
  m[, 3] = c(500, 1000, 5000)
  l = 4

  folders = c("mml_cpt", "mml_nb", "mml_rand", "iamb_bnlearn_0.01", "pcmb_0.01", "sll")
  for (folder in folders) {

    for (n in c(500, 1000, 5000)) {

      if (n == 1000) {
        ind = 2
      } else if (n == 5000) {
        ind = 3
      } else if (n == 500) {
        ind = 1
      }

      learned = list.files(paste0(model, "/", folder), paste0("_s", n, "_"))
      edList = rep(list(rep(0, nvars)), length(learned)) # edit distance
      retList = rep(list(c()), length(learned)) # precision and recall

      for (j in 1:length(learned)) {

        mbl = readRDS(paste0(model, "/", folder, "/", learned[j]))

        if (folder %in% c("mml_cpt", "mml_rand")) {
          mbl = symmetry_correction(vars, mbl, "union")
        } else if (folder %in% c("mml_nb")) {
          mbl = symmetry_correction(vars, mbl, "intersection")
        }

        for (k in 1:nvars) {
          edList[[j]][k] = mb_false_finding(mbt[[k]], mbl[[k]])
          retList[[j]] = c(retList[[j]], round(mb_retrieval(mbt[[k]], mbl[[k]], nvars), 1))
        }

      }# end for j

      avg_ed_over_all_nodes = paste0(round(conf_int(unlist(edList)), 1), collapse = "+-")
      retVec = unlist(retList)
      avg_pre_over_all_nodes = paste0(round(conf_int(retVec[!even(1:length(retVec))]), 2), collapse = "+-")
      avg_rec_over_all_nodes = paste0(round(conf_int(retVec[even(1:length(retVec))]), 2), collapse = "+-")
      avg_pre_rec_over_all_nodes = paste(avg_pre_over_all_nodes, avg_rec_over_all_nodes)
      m[ind, l] = avg_ed_over_all_nodes
      m[ind, l + 6] = avg_pre_rec_over_all_nodes
    }# end for n
    l = l + 1
  }# end for each method

  #write.table(m, "~/Documents/Experiments/kdd_exp/kdd_results.csv", sep = ",", append = T, col.names = F, row.names = F)

}

colnames(m) = c("model", "avg_mb", "n", rep(folders, 2))
m
