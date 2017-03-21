format_camml_prior = function(arc_list) {
  
  if (length(arc_list$single) == 2) arc_list$single = matrix(arc_list$single, 1, 2) 
  if (length(arc_list$bi) == 2) arc_list$bi = matrix(arc_list$bi, 1, 2) 
  
  text = "arcs {"
  
  if (nrow(arc_list$single) > 0) {
    
    for (i in 1:nrow(arc_list$single)) {
      
      text = paste(text, "\n", arc_list$single[i, 1], "->", arc_list$single[i, 2], prob, ";")      
      
    } # end for i
    
    if (nrow(arc_list$bi) > 0) {
      
      for (i in 1:nrow(arc_list$bi)) {
        
        text = paste(text, "\n", arc_list$bi[i, 1], "--", arc_list$bi[i, 2], prob, ";")      
        
      } # end for i
      
    } # end if 
    
    text = paste(text, "\n }")
    
  } # end if 
  
  return(text)
  
}