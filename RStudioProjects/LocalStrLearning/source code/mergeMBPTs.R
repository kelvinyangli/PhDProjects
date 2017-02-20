# this function merges mbdags into an entire dag 
# it could contain cycles or bi-directed arcs
mergeMBDags = function(mbpts, vars) {
  
  adjmtx_global = matrix(0, length(vars), length(vars), dimnames = list(vars, vars))
  
  for (k in 1:length(vars)) {
    
    if (nrow(mbpts[[k]]) > 1) { # when a var's mb is not empty 
      
      for (i in 1:nrow(mbpts[[k]])) {
        
        for (j in 1:ncol(mbpts[[k]])) {
          
          if (mbpts[[k]][i, j] == 1) adjmtx_global[rownames(mbpts[[k]])[i], colnames(mbpts[[k]])[j]] = 1
          
        } # end for j
        
      } # end for i
      
    } # end if 
    
  } # end for each var k 
  
  #if (isDag(adjmtx_global)) {
    
    return(adjmtx_global)
    
  #} else print("The resulting graph is not a dag!")
  
}

# we want to ranking different local strs before merging
# 1st priority is v-str 
# within v-str, the 1st priority is the one that can be confirmed by all its mb candidates
# e.g., if x->y<-z then y should be identified as a collider in all mb(x), mb(y) and mb(z) results 
# if y is confirmed as this, it is almost for sure a collier 
# once local strs are mergered together, we could stocastically search for better global str 

# this function finds the colliders in a list of local strs
# if a var has more than one parent, it is a collider 
findCollider = function(strList, vars) {
  
  colliders = c()
  for (i in 1:length(strList)) {
    
    for (j in 1:ncol(strList[[i]])) {
      
      # if a var has more than 1 parent, add its global index according to vars to the collider set
      if (sum(strList[[i]][, j]) > 1) colliders = c(colliders, which(vars == colnames(strList[[i]])[j]))
      
    } # end for j
    
  } # end for i 
  
  return(colliders)
  
}



