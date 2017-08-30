#' MML Naive Bayes using adaptive code approach
#'
#' This function calculates the mml score of a NB model using the adaptive code approach. It is much simpler than using
#' MML87 that involves complex fisher calculation. The output is the mml score of a NB without adding the extra bit 
#' for each parameter as what the multi-state mml adaptive code does, because we don't know the complete message length
#' from MML87 for NB. But maybe this is ok, because we are not interested in the mml estimation of parameters. 
#' @param data A categorical data set.
#' @param yIndex The target variable index.
#' @param xIndices A vector of indices for the Xs. 
#' @export
mml_nb_adaptive = function(data, yIndex, xIndices) {
  msgLen = 0
  ycnt = rep(1, nlevels(data[, yIndex]))
  if (length(xIndices) > 0) xcnt = rep(1, prod(arities[c(xIndices, yIndex)]))
  for (rowID in 1:nrow(data)) {
    yValue = as.numeric(data[rowID, yIndex])
    p = ycnt / sum(ycnt)
    if (length(xIndices) > 0) {
      xValues = as.numeric(data[rowID, xIndices])
      q = rep(0, length(xcnt))
      probX = 0
      for (i in 1:arities[yIndex]) {
        q[((i - 1) * prod(arities[xIndices]) + 1):(prod(arities[xIndices]) * i)] = 
          (xcnt[((i - 1) * prod(arities[xIndices]) + 1):(prod(arities[xIndices]) * i)] / 
             sum(xcnt[((i - 1) * prod(arities[xIndices]) + 1):(prod(arities[xIndices]) * i)])) * p[i]
        
        index = node_value_to_index(arities[c(xIndices, yIndex)], c(xValues, i))
        probX = probX + q[index]
        if (i == yValue) s = index
      }# end for each value of y
      msgLen = msgLen - log(q[s] / probX)
      xcnt[s] = xcnt[s] + 1
    } else {
      msgLen = msgLen - log(p[yValue]) 
    }
    ycnt[yValue] = ycnt[yValue] + 1
  }# end of each data point
  return(msgLen)
}