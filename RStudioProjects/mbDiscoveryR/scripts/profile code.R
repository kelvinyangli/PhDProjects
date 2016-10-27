# profile R code
Rprof("mml.out")
y = mbForwardSelection.fast(data, "CATECHOL", mmlCPT.fast, dataInfo$arities, dataInfo$indexListPerNodePerValue, base = 2)
Rprof(NULL)
proftable("mml.out")
