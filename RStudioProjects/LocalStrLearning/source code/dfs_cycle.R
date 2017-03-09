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

dfs_cycle = function(adjmtx, x, from, visited) {
  
  visited = c(visited, x)
  cat("visited:", visited, "\n")
  cat("from:", from, "\n")
  #from = x
  neighbour = unique(c(which(adjmtx[x, ] == 1), which(adjmtx[, x] == 1)))
  cat("nbr:", neighbour, "\n")
  
  for (i in 1:length(neighbour)) {
    
    current = neighbour[i]
    cat("current:", current, "\n")
    
    if (current != from) {
      cat("current != from \n")
      if (!current %in% visited) {
        cat("current not in visited \n")
        cat("add", current, "into", visited, "\n")
        cat("------------------------ \n")
        dfs_cycle(adjmtx, current, x, visited)
        
      } else {
        
        #ls = list(visited = visited, current = current, from = from)
        print("Cycle detected!")
        break 
        
      } # end else 
      
    } else {
      
      break
      
    } # end else 
    
    
  } # end for i 
  
  
  #return(ls)
  
}