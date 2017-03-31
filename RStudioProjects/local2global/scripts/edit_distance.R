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
    
    dir = paste0("../../../UAI_exp/", models[m], "/") 
    df = ed(methods, models[m], dir, n[s], nRepeat)
    cat(models[m], "-", n[s], "\n")
    cat(methods[1], ":", unlist(df[1, ]), "\n")
    cat(methods[2], ":", unlist(df[2, ]), "\n")
    cat("------------------------------- \n")
  }
  
}




