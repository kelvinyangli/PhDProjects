# here result is a txt file contains the ranking of all attributes
# each row corresponds to one attribute as a target 
# hence need to use the cptsTrue to get the true number of candiddates in mb

parseRelief = function(result, cptsTrue) {

  allNodes = names(cptsTrue)
  mbList = list()
  for (i in 1:length(cptsTrue)) {
    
    mbList[[i]] = unlist(result[i, ][1:length(mb(cptsTrue, allNodes[i]))])
    
  }
  
  return(mbList)
  
  
}