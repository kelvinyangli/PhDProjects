# check if arcs x and y are same, reversed, or different
# this function is called by editDistDags/hammingDags
checkArc = function(x, y) {
  
  if (prod(x == y)) {# when x and y are identical
    
    allEqual = 1
    
  } else if (prod(x == rev(y))) {# when x and y are in a reverse order
    
    allEqual = -1
    
  } else {# when x and y are not equal
    
    allEqual = 0
    
  }
  
  return(allEqual)
  
}