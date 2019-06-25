setwd("~/Documents/Experiments/mb_discovery_IJAR/80_5_4_1/")
for (n in c(2000)) {
  datasets = list.files(paste0("data_rds/", n))
  ## starting parallel computing ###

  for (ii in 2:length(datasets)) {
    data = readRDS(paste0("data_rds/", n, "/", datasets[ii]))
    filename = strsplit(datasets[ii], ".rds")[[1]]
    # nvars = ncol(data)
    vars = names(data)
    #data_cat = numeric2categorical(data)
    arities = sapply(data, nlevels)
    varCnt = count_occurance(data, arities)
    data = data.matrix(data)
    # registerDoParallel(4)
    # mbcpt = foreach(target = vars,
    #                 .combine = list,
    #                 .multicombine = TRUE) %dopar% {
    #                   forward_greedy_fast(data, varCnt, arities, vars, n, target, alpha=1)
    #                 }
    # names(mbcpt) = vars
    # stopImplicitCluster()
    # saveRDS(mbcpt, paste0("mml_cpt/", filename, ".rds"))
    # 
    # registerDoParallel(4)
    # mbnb = foreach(target = vars,
    #                .combine = list,
    #                .multicombine = TRUE) %dopar%
    #   forward_greedy(data, arities, vars, n, target, "nb")
    # names(mbnb) = vars
    # stopImplicitCluster()
    # saveRDS(mbnb, paste0("mml_nb/", filename))
    registerDoParallel(4)
    mbrand = foreach(target = vars,
        .combine = list,
        .multicombine = TRUE) %dopar%
      forward_greedy(data, arities, vars, n, target, "random", varCnt = varCnt, prior = "uniform")
    names(mbrand) = vars
    saveRDS(mbrand, paste0("mml_rand/", filename))
    stopImplicitCluster()
  }
}
