
associationMeasure = function(data, x, z = NULL, orderBy = "cmi", decreasing = FALSE, dropMML = TRUE) {
  var = names(data)
  unUsedNodes = var[!var %in% c(x, z)]
  vec.cmi = c()
  vec.pvalue = c()
  
  if (is.null(z)) {
    temp = sapply(unUsedNodes, ci.test, x = x, data = data)
  } else {
    temp = sapply(unUsedNodes, ci.test, x = x, z = z, data = data)
  }
  
  cmiList = unlist(temp["statistic",])
  pValueList = unlist(temp["p.value",])

  if (!dropMML) {
    mmlNodes = list()
    for (i in 1:length(unUsedNodes)) mmlNodes[[i]] = c(unUsedNodes[i], z)
    
    mmlTemp = sapply(mmlNodes, msgLen, data = data, indicatorMatrix, y = x)
    mmlList = unlist(mmlTemp["mml",])
  } else {
    mmlList = 0
  }
  
  df = data.frame(pValue = pValueList, cmi = cmiList, mml = mmlList)
  df = df[order(df[, orderBy], decreasing = decreasing),]
  total = apply(df, 2, sum)
  df = rbind(df, "sum" = total)  
  return(df)
}