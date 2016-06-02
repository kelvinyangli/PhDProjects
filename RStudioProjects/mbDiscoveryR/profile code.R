# profile R code
Rprof("mml.out")
y = mbGreedySearch(data, "V1", mmlCPT)
Rprof(NULL)
proftable("mml.out")