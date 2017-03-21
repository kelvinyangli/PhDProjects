find_bidirected_arcs = function(mtx) {
  
  ascending_order = t(apply(mtx, 1, order))
  mtx_ordered = matrix(0, nrow(mtx), 2)
  
  for (i in 1:nrow(mtx)) {
    
    mtx_ordered[i, ] = mtx[i, ][ascending_order[i, ]]
    
  }
  
  duplicated_arcs = mtx_ordered[which(duplicated(mtx_ordered) == TRUE), ]
  unique_arcs = mtx[!(duplicated(mtx_ordered) | duplicated(mtx_ordered[nrow(mtx_ordered):1, ])[nrow(mtx_ordered):1]),]
  
  return(list(single = unique_arcs, bi = duplicated_arcs))
  
}