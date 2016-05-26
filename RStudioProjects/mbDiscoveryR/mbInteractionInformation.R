mbInteractionInformation = function(data, x, ratio = 10, alpha = 0.05, debug = FALSE) {
  
  nodes = names(data)
  potentialNodes = nodes[nodes != x]

  parents = c()
  children = parents
  spouses = parents
  cpc = c(parents, children)
  cmb =c(parents, children, spouses)
  
  pValues = rep(0, length(potentialNodes))
  names(pValues) = potentialNodes
  CMIs = pValues
  informationRatio = pValues
  
  for (i in 1:length(potentialNodes)) {
    
    results = ci.test(x = x, y = potentialNodes[i], data = data)
      
    pValues[i] = results$p.value
    CMIs[i] = results$statistic
    
  } # end for
  
  # if all p values are greater than alpha, stop algorithm
  if (pValues[which.min(pValues)] >= alpha) break 

  # else add the index of the node with the highest cmi into cmb
  toAdd = which.max(CMIs)
  
  cpc = c(cpc, potentialNodes[toAdd])
  cmb = c(cpc, spouses)
  potentialNodes = potentialNodes[-toAdd]
  
  pValues = pValues[-toAdd]
  CMIs = CMIs[-toAdd]
  informationRatio = informationRatio[-toAdd]
  
  cat("* cpc is ", cpc, "\n")
  
  # repeat the process
  repeat {
    
    for (i in 1:length(potentialNodes)) {
      
      results = ci.test(x = x, y = potentialNodes[i], z = cmb, data = data)
        
      informationRatio[i] = results$statistic/CMIs[i]
      pValues[i] = results$p.value
      CMIs[i] = results$statistic
      
    } # end for 
    
    # if all p values are greater than alpha, stop algorithm
    if (pValues[which.min(pValues)] >= alpha) break 
    
    toAdd = which.max(CMIs)
    
    if (informationRatio[toAdd] > ratio) {
      spouses = c(spouses, potentialNodes[toAdd])
      cat("* spouses is ", spouses, "\n")
    } else {
      cpc = c(cpc, potentialNodes[toAdd])
      cat("* cpc is ", cpc, "\n")
    }
    
    potentialNodes = potentialNodes[-toAdd]
    
    pValues = pValues[-toAdd]
    CMIs = CMIs[-toAdd]
    informationRatio = informationRatio[-toAdd]
    
    cmb = c(cpc, spouses)
    
  } # end repeat
  mbList = list(mb = cmb, pc = cpc, spouses = spouses)
  print(mbList)
}






