# scripts to generate and save all possible mb dags
# dags will be saved in list and then saved into .rds into hard drive for furture reference 
# their names should follow the order of n_m_k, where n = |mb|, m = |colliders|, k = |spouses|
# for the case when there is no colliders and hence no spouses, the file will be named n_0_0

dir = "all mb dags/"
#x=c()
x = paste0("V", 1:7)
y = "T"
n = length(x)

# generate mb dags with no spouses
dagList = enumWithNoSp(x, y)
saveRDS(dagList, paste0(dir, n, "_0_0.rds"))
cat(n, "_0_0 :", length(dagList), "\n")

# generate mb dags with spouses
dag = empty.graph(c(x, y))
if (n > 1) {
  
  for (m in 1:floor(n / 2)) {
    
    for (k in 1:(n - 2 * m + 1)) {
      
      if (m < 2) {
        
        subDagList = readRDS(paste0(dir, n - k - 1, "_0_0.rds"))
        dagList = subEnumeration(x, y, m, k, subDagList)
        
        if (is.null(dagIsom(dagList))) {# apply dag isomorphism check, if pass then save into drive
          
          saveRDS(dagList, paste0(dir, n, "_", m, "_", k, ".rds"))
          cat(n, "_", m, "_", k, ":", length(dagList), "\n")
          
        } else {
          
          print("There are duplicated dags!")
          
        }
        
      } else {# when m >= 2
        
        dagList = c()
        
        for (k_dash in 1:min(k, n - k - 2)) {
          
          subDagList = readRDS(paste0(dir, n - k - 1, "_", m - 1, "_", k_dash, ".rds"))
          subList = subEnumeration(x, y, m, k, subDagList)
          
          if (k_dash == k) {# when there are duplicated dags
            # apply dag isomorphism check and remove the duplicated dags
            
            duplicatedIndices = dagIsom(subList)[, 2]
            subList = subList[-duplicatedIndices]
            
          }
          
          dagList = c(dagList, subList)
          
        } # end for each k_dash
        
        if (is.null(dagIsom(dagList))) {# apply dag isomorphism check, if pass then save into drive
          
          saveRDS(dagList, paste0(dir, n, "_", m, "_", k, ".rds"))
          cat(n, "_", m, "_", k, ":", length(dagList), "\n")
          
        } else {
          
          print("There are duplicated dags!")
          
        }
        
      } # end else when m >= 2
      
    } # end for k 
    
  } # end for m
  
} # end if m > 1



