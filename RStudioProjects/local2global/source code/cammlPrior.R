# a function to combine arcs and corresponding priors measure by bootstrapping and output a txt file for camml to read in
# as a prior 
cammlPrior = function(arcs, priors) {
  
  #if (length(arc_list$single) == 2) arc_list$single = matrix(arc_list$single, 1, 2) 
  #if (length(arc_list$bi) == 2) arc_list$bi = matrix(arc_list$bi, 1, 2) 
  
  text = "arcs {"
  
  if (length(arcs$directed) > 0) {
    
    for (i in 1:nrow(arcs$directed)) {
      
      text = paste(text, "\n", arcs$directed[i, 1], "->", arcs$directed[i, 2], priors$directed[i], ";")
    
    } # end for i 
    
    
  }
  
  if (length(arcs$undirected) > 0) {
    
    for (i in 1:nrow(arcs$undirected)) {
      
      text = paste(text, "\n", arcs$undirected[i, 1], "--", arcs$undirected[i, 2], priors$undirected[i], ";")
      
    } # end for i 
    
  }
  
  text = paste(text, "\n }")
    
}


#############################################################################################
cammlPrior = function(arcs, priors) {
  
  #if (length(arc_list$single) == 2) arc_list$single = matrix(arc_list$single, 1, 2) 
  #if (length(arc_list$bi) == 2) arc_list$bi = matrix(arc_list$bi, 1, 2) 
  
  text = "arcs {"
  
  if (length(arcs) > 0) {
    
    for (i in 1:nrow(arcs)) {
      
      text = paste(text, "\n", arcs[i, 1], "->", arcs[i, 2], priors$directed[i], ";")
      text = paste(text, "\n", arcs[i, 1], "--", arcs[i, 2], priors$undirected[i], ";")
      
    } # end for i 
    
    
  }
  
  text = paste(text, "\n }")
  
}


