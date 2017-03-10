nvars = 12
maxNPa = 2
maxArity = 3
n = 5000
beta = 1
maxMB = 7

#mbptsList = list()
#for (i in 1:8) mbptsList[[i]] = readRDS(paste0("MBPTs/", i - 1, ".rds")) 

# log factorial sheet
#logFactorialSheet = read.csv("logFactorial_1to10000.csv")

par(mfrow = c(1, 3))
dag = randAdjmtx(nvars, maxNPa)
dag_bn = matrix2dag(dag)
graphviz.plot(dag_bn, main = "true")
cpts = randCPTs(dag_bn, maxArity, beta)
data = rbn(cpts, n)
dataInfo = getDataInfo(data)
vars = colnames(data)

mbList = list()
for (i in 1:length(vars)) mbList[[i]] = mbForwardSelection.fast(data, vars[i], dataInfo$arities, dataInfo$indexListPerNodePerValue, base = exp(1))
mbList = symmetryCorrection(vars, mbList) # apply symmetry correction 

for (i in 1:nvars) if (length(mbList[[i]]) > maxMB) mbList[[i]] = mbList[[i]][1:maxMB]

learned = learnMBPT(vars, mbList, mbptsList, dataInfo, n)
mbpt_global = learned$mbpt
graphviz.plot(matrix2dag(mbpt_global), main = "mml")
graphviz.plot(mmhc(data, score = "bde"), main = "mmhc")

editDistDags(matrix2dag(mbpt_global), dag_bn)
editDistDags(mmhc(data, score = "bde"), dag_bn)


localStrs = learned$localStrs
# create prior for camml
vars = colnames(data)
prob = 0.8
file = "prior.txt"
text = "arcs {"
for (k in 1:length(localStrs)) {
  
  for (i in 1:nrow(localStrs[[k]])) {
    
    for (j in 1:ncol(localStrs[[k]])) {
      
      if (localStrs[[k]][i, j] == 1) {
        
        text = paste(text, "\n", rownames(localStrs[[k]])[i], "->", colnames(localStrs[[k]])[j], prob, ";")
        
      } # end if 
      
    } # end for j 
    
    
  } # end for i 
  
}
text = paste(text, "\n }")
write_file(text, file)

# plot
res_withPrior = netica2bnlearn("test_camml0.dne")
dag_camml_withPrior = parentsList2BN(res_withPrior)
graphviz.plot(dag_camml_withPrior)








