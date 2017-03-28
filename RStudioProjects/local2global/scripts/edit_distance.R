# computing edit distance b/w the learned and true polytrees 
# here the edit distance is for dag, pattern and skeleton
methods = c("global_pt", "camml_noPrior")
#methods = c("global_pt")
#methods = c("global_pt")
dir = "../../../UAI_exp/insurance/"
real = TRUE
n = 500
nRepeat = 10
dags = list.files(paste0(dir, "dag"), ".rds")
mtx = matrix(0, nRepeat * length(dags), length(methods))
for (k in 1:length(methods)) {
  
  #cnt = 1
  dags_learned = list.files(paste0(dir, methods[k], "/", n, "/"))
  
  for (i in 1:length(dags)) {
    
    if (real) {
      
      dag = readRDS(paste0(dir, "dag/", dags[i]))
      
    } else {
      
      adjmtx = readRDS(paste0(dir, "dag/", dags[i]))$adjmtx
      dag = matrix2dag(adjmtx)
    
    }
    
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
        
      mtx[j, k] = editDistDags(learned, dag) # pattern
      #mtx[j, k] = bnlearn::shd(matrix2dag(dag_sa), dag) # pattern
      #mtx[j, k] = editDistDags(learned, dag) # dag
  
      #}
      
    } # end for j
    
  } # end for i
  
} # end for k

colnames(mtx) = methods
colMeans(mtx)
apply(mtx, 2, computeCI)
#colMeans(mtx[1:25,])
#apply(mtx[1:25,], 2, computeCI)
#colMeans(mtx[26:50,])
#apply(mtx[26:50,], 2, computeCI)




