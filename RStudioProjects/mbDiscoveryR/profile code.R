# profile R code
Rprof("mml.out")
y = mbForwardSelection(data,"V5",mmlLogit2ndOrder,datainfo$arities,datainfo$indexListPerNodePerValue,indicatorMatrix = indicatorMatrix, interaction = T, debug=F)
Rprof(NULL)
proftable("mml.out")
