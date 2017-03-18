# computing edit distance b/w the learned and true polytrees 
# here the edit distance is for dag, pattern and skeleton
methods = c("camml_withPrior", "camml_noPrior", "camml_unfixedPrior", "mmhc_bnstruct", "mmhc_bnlearn", "tabu_bde_bnlearn")
dir = "../../../Dag experiments/"
n = 1000
nVars = 20
nRepeat = 5
pts_true = list.files(paste0(dir, "dag"), paste0(nVars, "_"))
ed_dag = ed_sklt = ed_pattern = c()
ed_dag_temp = ed_sklt_temp = ed_pattern_temp = matrix(0, nrow = length(pts_true), ncol = length(n))
total_ed_dag = matrix(0, 25, length(methods), dimnames = list(NULL, methods))

for (k in 1:length(methods)) {
  
  cnt = 1
  pts_learned = list.files(paste0(dir, methods[k], "/", n, "/"), paste0(nVars, "_"))
  
  for (i in 1:length(pts_true)) {
    
    x = y = z = c()
    pt_true = readRDS(paste0(dir, "dag/", pts_true[i]))
    pt_true = matrix2dag(pt_true$adjmtx)
    #filename = strsplit(pts_true[i], ".rds")[[1]][1]
    #filename = paste0(filename, "_", n[k])
    
    
    for (j in ((i - 1) * nRepeat + 1):(i * nRepeat)) {
      
      if (length(grep("camml", methods[k])) > 0) {
        
        parentsList = netica2bnlearn(paste0(dir, methods[k], "/", n, "/", pts_learned[j]))
        pt_learned = parentsList2BN(parentsList)
        
      } else if (methods[k] == "mmhc_bnstruct") {
        
        pt_learned = matrix2dag(readRDS(paste0(dir, methods[k], "/", n, "/", pts_learned[j])))
        
      } else {
        
        pt_learned = readRDS(paste0(dir, methods[k], "/", n, "/", pts_learned[j]))
        
      }
      
      # compute edit distance b/w learned and true 
      x = c(x, bnlearn::shd(pt_learned, pt_true)) # pattern
      y = c(y, bnlearn::hamming(pt_learned, pt_true)) # skeleton
      z = c(z, editDistDags(pt_learned, pt_true)) # dag
      
      total_ed_dag[cnt, k] = editDistDags(pt_learned, pt_true)
      cnt = cnt + 1
      
    } # end for j
    
    #write.csv(x, paste0(dir, "editDistance/", method, "/pattern/", filename, ".csv"), row.names = FALSE)
    #write.csv(y, paste0(dir, "editDistance/", method, "/skeleton/", filename, ".csv"), row.names = FALSE)
    #write.csv(z, paste0(dir, "edit_distance/", method, "/", filename, ".csv"), row.names = FALSE)
    
    ed_pattern_temp[i, 1] = mean(x)
    ed_sklt_temp[i, 1] = mean(y)
    ed_dag_temp[i, 1] = mean(z)
    
  } # end for i
  
  ed_dag = cbind(ed_dag, ed_dag_temp)
  ed_sklt = cbind(ed_sklt, ed_sklt_temp)
  ed_pattern = cbind(ed_pattern, ed_pattern_temp)
  
} # end for k

colnames(ed_dag) = colnames(ed_sklt) = colnames(ed_pattern) = methods

mean_ed_dag = colMeans(ed_dag)
mean_ed_sklt = colMeans(ed_sklt)
mean_ed_pattern = colMeans(ed_pattern)

mean_ed_dag[order(mean_ed_dag)]
mean_ed_sklt[order(mean_ed_sklt)]
mean_ed_pattern[order(mean_ed_pattern)]

total_ed_dag

upper = apply(total_ed_dag, 2, mean) + 1.96 * apply(total_ed_dag, 2, sd)/sqrt(25)
lower = apply(total_ed_dag, 2, mean) - 1.96 * apply(total_ed_dag, 2, sd)/sqrt(25)

names(upper) = names(lower) = c()

df = data.frame(mean = colMeans(total_ed_dag), lower, upper)

ord = order(colMeans(total_ed_dag))
(round(df[ord, ], 2))

#write.csv(ed_dag, paste0(dir, "edit_distance/", method, ".csv"), row.names = FALSE)
#colnames(ed_sklt) = colnames(ed_pattern) = colnames(ed_dag) = n
#ed_dag = ed_dag[1:20,]
#ed_sklt = ed_sklt[1:20,]
#ed_pattern = ed_pattern[1:20,]



