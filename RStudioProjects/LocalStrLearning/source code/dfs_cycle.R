# since the resulting globla mbpt could have multiple paths b/w two variables
# when merging local polytrees, we decide to drop a edge in the undirected 
# cycle, which results in a lower mml score. 
# this is equivalent to drop the arc with the highest mml score
# 
# need a function to check for multi paths b/w two vars 
# multiPaths(adjmtx) -> return a list of undirected cycles, where each level contains the vars in the cycle
# 
# based on these results, we could measure the mml(adjmtx) by deleting each arc/path
# 
# and hopefully select  the one that decrease the mml the most 
# 
# if not then the one that increase the mml the least

#visited = rep(FALSE, ncol(adjmtx))

dfs_cycle = function(adjmtx, x, from, visited) {
  
  #visited = c(visited, x)
  cat("visited:", colnames(adjmtx[visited]), "\n")
  
  neighbour = unique(c(which(adjmtx[x, ] == 1), which(adjmtx[, x] == 1)))
  neighbour = neighbour[order(neighbour)]
  
  cat("nbr(", x, "):", neighbour, "\n")
  cat("from:", from, "\n")
  
  if ((length(neighbour) > 1) && (prod(neighbour %in% visited))) {
    
    cat("cycle detected! \n")
    return(0) 
    
  } else {
    
    
    for (i in 1:length(neighbour)) {
      
      current = neighbour[i]
      cat("current:", current, "\n")
      
      if ((current != from) && (current %in% visited)) {
        
        visited = visited
        from = 
        x = 
        dfs_cycle(adjmtx, x, from, visited)
        cat(current, "has been visited! skip \n")
        #cat("Cycle detected! \n")
        #break 
        
      } else if ((current != from) && (!current %in% visited)) {
        
        cat("current not in visited \n")
        cat("add", current, "into", visited, "\n")
        
        visited = c(visited, current)
        from = x
        x = current
        cat("visited:", visited, "\n")
        cat("------------------------ \n")
        dfs_cycle(adjmtx, x, from, visited)
        
      } else if (current == from) {
        
        cat("current = from, skip \n")
        
      } 
      
    } # end for i 
    
  } # end else 
  
  #return(0)
  
}
















