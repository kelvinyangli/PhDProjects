inVStructure = function(data, node) {
  
  allNodes = colnames(data)
  X = allNodes[allNodes != node]
  
  cmi = vector(length = length(X))
  
  ss = rep(0, length(X))
  
  for (i in 1:length(X)) {
    
    Y = X[-i]
    
    ss[i] = ci.test(x = node, y = X[i], data = data)$statistic
    
    s = 0
    for (j in 1:length(Y)) {
      
      s = s + ci.test(x = node, y = Y[j], z = X[i], data = data)$statistic
      
    }
    
    cmi[i] = s
    
  }
  
  df = rep(0, length(ss))
  
  for (i in 1:length(ss)) {
    
    df[i] = sum(ss[-i])
    
  }
  
  df = cbind(cmi, df, cmi - df, cmi/df)
  
  colnames(df) = c("cmi","ref cmi", "relative change", "relative ratio")
  rownames(df) = X
  
  return(df)
  
}


