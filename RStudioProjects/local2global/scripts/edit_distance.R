# computing edit distance b/w the learned and true polytrees 
# here the edit distance is for dag, pattern and skeleton
<<<<<<< HEAD
methods = c("global_pt", "camml_unfixedPrior", "camml_noPrior", "mmhc_bnlearn", "tabu_bde_bnlearn")
dir = "../../../UAI_exp/"
n = 1000
nRepeat = 5
pts_true = list.files(paste0(dir, "dag"), ".rds")
ed_dag = ed_sklt = ed_pattern = c()
ed_dag_temp = ed_sklt_temp = ed_pattern_temp = matrix(0, nrow = length(pts_true), ncol = length(n))
total_ed_dag = matrix(0, 100, length(methods), dimnames = list(NULL, methods))

=======
methods = c("global_pt", "camml_noPrior", "mmhc_bnlearn")
#methods = c("global_pt")
dir = "../../../UAI_exp/"
n = 1000
nRepeat = 5
dags = list.files(paste0(dir, "dag"), ".rds")[1:10]
mtx = matrix(0, nRepeat * length(dags), length(methods))
>>>>>>> master
for (k in 1:length(methods)) {
  
  cnt = 1
  dags_learned = list.files(paste0(dir, methods[k], "/", n, "/"))[1:50]
  
  for (i in 1:length(dags)) {
    
    #x = y = z = c()
    #z = c()
    adjmtx = readRDS(paste0(dir, "dag/", dags[i]))$adjmtx
    dag = matrix2dag(adjmtx)
    
    for (j in ((i - 1) * nRepeat + 1):(i * nRepeat)) {
      
      if (length(grep("camml", methods[k])) > 0) {
        
        parentsList = netica2bnlearn(paste0(dir, methods[k], "/", n, "/", dags_learned[j]))
        learned = parentsList2BN(parentsList)
        
      } else {
        
        adjmtx_learned = readRDS(paste0(dir, methods[k], "/", n, "/", dags_learned[j]))
        
      }
        
      if (methods[k] == "global_pt") learned = matrix2dag(adjmtx_learned)
      filename = strsplit(dags_learned[j], ".rds")[[1]][1]
      data = read.csv(paste0(dir, "data_csv/", n, "/", filename, ".csv"))
      dataInfo = getDataInfo(data)
      if (!directed(learned)) {# if the learned is cpdag 
        
        dag_learned = cpdag2dag(learned, adjmtx_learned, dataInfo, colnames(data), n)
        
      }
      
<<<<<<< HEAD
      if (methods[k] == "global_pt") {
        
        pt_learned = matrix2dag(pt_learned)
        
      }
      
      # compute edit distance b/w learned and true 
      #x = c(x, bnlearn::shd(pt_learned, pt_true)) # pattern
      #y = c(y, bnlearn::hamming(pt_learned, pt_true)) # skeleton
      z = c(z, editDistDags(pt_learned, pt_true)) # dag
=======
>>>>>>> master
      
      dag_sa = sa(dag_learned$adjmtx, colnames(data), dataInfo, n, step = 0.01, maxItr = 100)
      #if (acyclic(cextend(learned))) {
        
      mtx[j, k] = bnlearn::shd(matrix2dag(dag_sa), dag) # pattern
      #mtx[j, k] = editDistDags(learned, dag) # dag
  
      #}
      
    } # end for j
    
  } # end for i
  
} # end for k

<<<<<<< HEAD
colnames(ed_dag) = methods

#mean_ed_dag = colMeans(ed_dag)
#mean_ed_sklt = colMeans(ed_sklt)
#mean_ed_pattern = colMeans(ed_pattern)

#mean_ed_dag[order(mean_ed_dag)]
#mean_ed_sklt[order(mean_ed_sklt)]
#mean_ed_pattern[order(mean_ed_pattern)]

#write.csv(total_ed_dag, paste0(dir, "edit_distance/ed_dag_", n, ".csv"), row.names = F)

=======
colnames(mtx) = methods
colMeans(mtx[1:25,])
apply(mtx[1:25,], 2, computeCI)
colMeans(mtx[26:50,])
apply(mtx[26:50,], 2, computeCI)
>>>>>>> master




