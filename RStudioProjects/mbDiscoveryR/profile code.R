# profile R code
Rprof("mml.out")
y = mbForwardSelection(data,"V5",mmlLogit,dd$arities,dd$indexListPerNodePerValue,indicatorMatrix = indicatorMatrix, debug=F)
Rprof(NULL)
proftable("mml.out")
