# learn mbs using bnlearn's iamb
setwd("~/Documents/Experiments/mb_discovery_IJAR/")

# real models
for (model in c("alarm", "child", "insurance", "hailfinder", "barley")) {
  #model = "insurance"
  dag = readRDS(paste0(model, "/dag/", model, ".rds"))
  vars = bnlearn::nodes(dag)
  mbt = sapply(vars, bnlearn::mb, x = dag)
  for (n in c(500, 1000, 5000)) {
    datasets = list.files(paste0(model, "/data_csv/", n))
    for (i in 1:length(datasets)) {
      name = strsplit(datasets[i], ".csv")[[1]]
      data = read.csv(paste0(model, "/data_csv/", n, "/", datasets[i]))
      #vars = colnames(data)
      dt = numeric2categorical(data)
      for (k in 1:ncol(dt)) {
        if (nlevels(dt[, k]) < 2) {
          levels(dt[, k]) = c("0", "1")
        }
      }
      mb_iamb1 = sapply(vars, bnlearn::learn.mb, x = dt, method = "iamb", alpha = 0.01)
      saveRDS(mb_iamb1, paste0(model, "/iamb_bnlearn_0.01/", n, "/", datasets[i], ".rds"))
      # mb_iamb5 = sapply(vars, bnlearn::learn.mb, x = dt, method = "iamb", alpha = 0.05)
      # e1 = e5 = rep(0, length(vars))
      # for (j in 1:length(vars)) {
      #   e1[j] = edit_dist_mb(mbt[[j]], mb_iamb1[[j]], vars, vars[j])
      #   e5[j] = edit_dist_mb(mbt[[j]], mb_iamb5[[j]], vars, vars[j])
      # }
      # cat(sum(e1)-sum(e5), "\n")
    }
  }
}

# artificial models
model = "80_5_4_1"
for (n in c(100, 500, 2000, 5000)) {
  datasets = list.files(paste0(model, "/data_rds/", n))
  for (i in 1:length(datasets)) {
    data = readRDS(paste0(model, "/data_rds/", n, "/", datasets[i]))
    vars = colnames(data)
    mb_iamb1 = sapply(vars, bnlearn::learn.mb, x = data, method = "iamb", alpha = 0.01)
    saveRDS(mb_iamb1, paste0(model, "/iamb_bnlearn_0.01/", datasets[i]))
  }
}

