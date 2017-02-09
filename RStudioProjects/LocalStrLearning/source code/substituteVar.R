# this function substitute the var names in the true/learned mb into vars in the pre-saved mbpts
# ls is a list of mbpts whose var names are v_1, ..., v_n 
# these vars will be replaced by z, which is a list of true/learned mb candidates
# x is the target var
substituteVar = function(ls, x, z) {
  
  xIndex = ncol(ls[[1]])
  
  for (i in 1:length(ls)) {
    
    #nodes(ls[[i]])[nodes(ls[[i]]) != y][order(nodes(ls[[i]])[nodes(ls[[i]]) != y])] = z
    colnames(ls[[i]])[-xIndex][order(colnames(ls[[i]])[-xIndex])] = z
    colnames(ls[[i]])[xIndex] = x
    rownames(ls[[i]]) = colnames(ls[[i]])
    
  } # end for i 
  
  return(ls)
  
}