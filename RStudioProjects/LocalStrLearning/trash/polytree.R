# this function searches for the ancestors of a variable from the global adjacency matrix 
# an ancestor of a var is a var that is either its parent, or its grandparent, or its grand-grandparent, etc...
ancestors = function(var, adjmtx) {
  
  parents = which(adjmtx[, var] == 1)
  ancsts = parents
  repeat {
    
    temp = c()
    # search for parents of each node in current parent set
    for (i in 1:length(parents)) temp = c(temp, which(adjmtx[, parents[i]] == 1))
    if (length(temp) == 0) break # stop searching when the current children are root nodes
    parents = temp # otherwise assign temp to parents and repeat the searching process
    ancsts = c(ancsts, temp)
      
  } # end repeat
  
  names(ancsts) = c() # get rid of vector names
  return(ancsts)
  
}

# this function searches for the descendents of a variable from the global adjacency matrix
# an descendent of a var is a var that is either its child, or its grandchild, or its grand-grandchild, etc...
descendents = function(var, adjmtx) {
  
  children = which(adjmtx[var, ] == 1) # children of var
  descdts = children
  repeat {
    
    temp = c()
    # search for children of each node in current child set
    for (i in 1:length(children)) temp = c(temp, which(adjmtx[children[i], ] == 1))
    if (length(temp) == 0) break # stop searching when the current children are leave nodes 
    children = temp # otherwise assign temp to children and repeat the searching process
    descdts = c(descdts, temp)
    
  } # end repeat
  
  names(descdts) = c() # get rid of vector names
  return(descdts)
  
}
  
# this function searches for the non-descendents of a variable from the global adjacency matrix
# the non-descendents of a var is all nodes except its descendents
nonDescendents = function(var, adjmtx) {
  
  descdts = descendents(var, adjmtx) # get descendents of var
  indices = 1:ncol(adjmtx) # indices of all vars
  nonDes = indices[-c(which(colnames(adjmtx) == var), descdts)] # remove var and its descendents
  return(nonDes)
  
}


# this function takes the number of variables and the maximum number of parents as inputs 
# and generate a random polytree 
polytree = function(nVars, maxNPa) {
  
  vars = paste0("V", 1:nVars)
  mtx = matrix(0, nVars, nVars)
  dimnames(mtx) = list(vars, vars)
  
  
  for (i in 2:nVars) {
    
    nPa = min(maxNPa, sample(0:(i - 1), 1)) # sample the number of parents from [0, n vars preceding]
    if (nPa > 0) pa = sample(1:(i - 1), nPa) # when nPa is non-zeor, sample nPa parents from preceding vars
    
    if (nPa > 0) {
      
      for (j in 1:nPa) {
        
        ancst_i = ancestors(i, mtx)
        ancst_j = ancestors(pa[j], mtx)
        commonAncst = intersect(ancst_i, ancst_j) 
        if ((length(commonAncst) < 1) && (!pa[j] %in% ancst_i)) mtx[pa[j], i] = 1
        
      } # end for j 
      
    } # end if 
    
  } # end for i 
  
  return(mtx)
  
}












