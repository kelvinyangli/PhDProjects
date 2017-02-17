# this is a function to identify a path b/w two vars
f = function(x, y, vars, adjmtx) {
  
  indexX = which(vars == x)
  indexY = which(vars == y)
  
  paX = which(adjmtx[, indexX] == 1) # find pa(x)
  
}

adjmtx = randAdjmtx(5, 2)
graphviz.plot(matrix2dag(adjmtx))
adjmtx_undirected = adjmtx + t(adjmtx) # convert the directed graph into an undirected graph
vars = colnames(adjmtx)
f(2,5, vars, adjmtx_undirected)

# x and y are indices not the actual variable names
# adjmtx_undirected is the adjmtx of the original graph without directions
f = function(x, y, visited, vars, adjmtx_undirected) {
  
  current = x # initialize current visit as x
  visited = c(x visited)
  existPath = 0
  
  # start searching from x 
  nbr = which(adjmtx_undirected[x, ] == 1) # since edges are undirected, so don't know if a nbr is a parent or child
  for (i in 1:length(nbr)) {
    
    if (!nbr[i] %in% visited) {
      
      current = nbr[i] # update the current visit
      #visited = x # update visited nodes 
      
      if (current == y) { # if current is y then stop search and confirm there is a path b/w x--y
        
        existPath = 1
        break
        
      } else {
        
        existPath = f(current, y, visited, vars, adjmtx_undirected)
        
      } # end else 
      
    } # end if 
    
  } # end for i 
  
  return(existPath)
  
}
