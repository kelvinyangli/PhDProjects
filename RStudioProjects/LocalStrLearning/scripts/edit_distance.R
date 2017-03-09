# computing edit distance b/w the learned and true polytrees 
# here the edit distance is for dag, pattern and skeleton
method = "mml"
dir = "../../../Polytree experiments/"
n = c(1000, 10000)
nRepeat = 10
pts_true = list.files(paste0(dir, "pt"))
#m = nRepeat * length(n)
ed_dag = ed_sklt = ed_pattern = matrix(0, nrow = length(pts_true), ncol = length(n))

for (k in 1:length(n)) {
  
  pts_learned = list.files(paste0(dir, "pt_", method), paste0("_", n[k], "_"))
  
  #for (i in 1:length(pts_true)) {
  for (i in 1:20) {
    
    x = y = z = c()
    pt_true = readRDS(paste0(dir, "pt/", pts_true[i]))
    pt_true = matrix2dag(pt_true$adjmtx)
    filename = strsplit(pts_true[i], ".rds")[[1]][1]
    filename = paste0(filename, "_", n[k])
    
    for (j in ((i - 1) * nRepeat + 1):(i * nRepeat)) {
      
      pt_learned = readRDS(paste0(dir, "pt_", method, "/", pts_learned[j]))  
      if (method == "mml") pt_learned = matrix2dag(pt_learned)
      
      # compute edit distance b/w learned and true 
      x = c(x, shd(pt_learned, pt_true)) # pattern
      y = c(y, hamming(pt_learned, pt_true)) # skeleton
      z = c(z, editDistDags(pt_learned, pt_true)) # dag
      
    } # end for j
    
    write.csv(x, paste0(dir, "editDistance/", method, "/pattern/", filename, ".csv"), row.names = FALSE)
    write.csv(y, paste0(dir, "editDistance/", method, "/skeleton/", filename, ".csv"), row.names = FALSE)
    write.csv(z, paste0(dir, "editDistance/", method, "/dag/", filename, ".csv"), row.names = FALSE)
    
    ed_pattern[i, k] = mean(x)
    ed_sklt[i, k] = mean(y)
    ed_dag[i, k] = mean(z)
    
  } # end for i
  
} # end for k

colnames(ed_sklt) = colnames(ed_pattern) = colnames(ed_dag) = n
ed_dag = ed_dag[1:20,]
ed_sklt = ed_sklt[1:20,]
ed_pattern = ed_pattern[1:20,]



