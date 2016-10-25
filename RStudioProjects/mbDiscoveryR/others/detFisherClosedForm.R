detFisher2 = function(dependVar, independVar, beta, nFreePar = nFreePar) {
  index = which(dependVar == 1)
  tempData = cbind(dependVar[index], independVar[index,])
  
  for (i in 3:ncol(tempData)) {
    
  }
}

f = function(independVar) {
  
}

p = function(beta, independVar) {
  betaDotX = sum(beta*independVar)
  1/(1 + exp(-betaDotX))
}



