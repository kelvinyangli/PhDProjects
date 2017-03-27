# computing edit distance b/w the learned and true polytrees 
# here the edit distance is for dag, pattern and skeleton
methods = c("global_pt", "camml_noPrior", "mmhc_bnlearn")
#methods = c("global_pt")
dir = "../../../UAI_exp/"
n = 1000
nRepeat = 5
pts_true = list.files(paste0(dir, "dag"), ".rds")
mtx = matrix(0, nRepeat * length(pts_true), length(methods))
for (k in 1:length(methods)) {
  
  cnt = 1
  pts_learned = list.files(paste0(dir, methods[k], "/", n, "/"))
  
  for (i in 1:length(pts_true)) {
    
    #x = y = z = c()
    #z = c()
    pt_true = readRDS(paste0(dir, "dag/", pts_true[i]))
    pt_true = matrix2dag(pt_true$adjmtx)
    
    for (j in ((i - 1) * nRepeat + 1):(i * nRepeat)) {
      
      if (length(grep("camml", methods[k])) > 0) {
        
        parentsList = netica2bnlearn(paste0(dir, methods[k], "/", n, "/", pts_learned[j]))
        pt_learned = parentsList2BN(parentsList)
        
      } else {
        
        pt_learned = readRDS(paste0(dir, methods[k], "/", n, "/", pts_learned[j]))
        
      }
        
      if (methods[k] == "global_pt") pt_learned = matrix2dag(pt_learned)
      
      #if (acyclic(cextend(pt_learned))) {
        
      #mtx[j, k] = bnlearn::shd(pt_learned, pt_true) # pattern
      mtx[j, k] = editDistDags(pt_learned, pt_true) # dag
  
      #}
      
    } # end for j
    
  } # end for i
  
} # end for k

colnames(mtx) = methods
colMeans(mtx)
apply(mtx, 2, computeCI)




