# pcmb learn mb

# real models
for (model in c("alarm", "child", "insurance", "hailfinder", "barley")) {
  #model = "insurance"
  # dag = readRDS(paste0(model, "/dag/", model, ".rds"))
  # vars = bnlearn::nodes(dag)
  # mbt = sapply(vars, bnlearn::mb, x = dag)
  for (n in c(500, 1000, 5000)) {
    datasets = list.files(paste0(model, "/data_csv/", n))
    for (i in 1:length(datasets)) {
      name = strsplit(datasets[i], ".csv")[[1]]
      data = read.csv(paste0(model, "/data_csv/", n, "/", datasets[i]))
      vars = colnames(data)
      nvars = length(vars)
      mb_pcmb = system(paste0("./kmb4_linux ", model, "/data_csv/", n, "/", datasets[i], " ", n, " ", nvars, " -1 1.0 0 1 0.01"), intern = TRUE)
      mb_pcmb = pcmb2list(mb_pcmb, vars, "pcmb")
      saveRDS(mb_pcmb, paste0(model, "/pcmb_0.01/", name, ".rds"))
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
