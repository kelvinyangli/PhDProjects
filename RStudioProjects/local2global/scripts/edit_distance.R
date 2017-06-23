# computing edit distance b/w the learned and true polytrees 
# here the edit distance is for dag, pattern and skeleton
#methods = c("global_pt", "camml_noPrior", "mmhc")
methods = c("global_pt", "camml_noPrior")
real = TRUE
pattern = TRUE
n = c(500, 1000, 5000)
nRepeat = 10
models = c("child")
mtx = c()
for (m in 1:length(models)) {
  
  for (s in 1:length(n)) {
    
    for (j in ((i - 1) * nRepeat + 1):(i * nRepeat)) {
      
      if (length(grep("camml", methods[k])) > 0) {
        
        parentsList = netica2bnlearn(paste0(dir, methods[k], "/", n, "/", dags_learned[j]))
        learned = parentsList2BN(parentsList)
        
      } else {
        
        adjmtx_learned = readRDS(paste0(dir, methods[k], "/", n, "/", dags_learned[j]))
        
      }
        
      if (methods[k] == "global_pt") learned = matrix2dag(adjmtx_learned)
      #filename = strsplit(dags_learned[j], ".rds")[[1]][1]
      #data = read.csv(paste0(dir, "data_csv/", n, "/", filename, ".csv"))
      #if (real) data = numeric2Nominal(data)
      #dataInfo = getDataInfo(data)
      #if (!directed(learned)) {# if the learned is cpdag 
        
      #  dag_learned = cpdag2dag(learned, adjmtx_learned, dataInfo, colnames(data), n)
        
      #}
      
      #dag_sa = sa(dag_learned$adjmtx, colnames(data), dataInfo, n, step = 0.01, maxItr = 100)
      #if (acyclic(cextend(learned))) {
        
      mtx[j, k] = editDistDags(learned, dag) # dag
      #mtx[j, k] = bnlearn::shd(matrix2dag(dag_sa), dag) # pattern
      #mtx[j, k] = editDistDags(learned, dag) # dag
  
      #}
      
    } # end for j
    
  } # end for i
  
} # end for k

colnames(mtx) = methods
mtx
cat(colMeans(mtx))
#write.table(mtx, "../../../UAI_exp/insurance.txt", append = FALSE, row.names = FALSE)
#apply(mtx, 2, computeCI)
#colMeans(mtx[1:25,])
#apply(mtx[1:25,], 2, computeCI)
#colMeans(mtx[26:50,])SS
#apply(mtx[26:50,], 2, computeCI)
=======
    dir = paste0("../../../UAI_exp/", models[m], "/") 
    df = ed(methods, models[m], dir, n[s], nRepeat)
    cat(models[m], "-", n[s], "\n")
    cat(methods[1], ":", unlist(df[1, ]), "\n")
    cat(methods[2], ":", unlist(df[2, ]), "\n")
    cat("------------------------------- \n")
  }
  
}
>>>>>>> master




