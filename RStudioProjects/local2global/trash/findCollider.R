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
  
  return(unique(colliders))
  
}