#' MML Naive Bayes using adaptive code approach
#'
#' This function calculates the mml score of a NB model using the adaptive code approach. It is much simpler than using
#' MML87 that involves complex fisher calculation. The output is the mml score of a NB without adding the extra bit 
#' for each parameter as what the multi-state mml adaptive code does, because we don't know the complete message length
#' from MML87 for NB. But maybe this is ok, because we are not interested in the mml estimation of parameters. 
#' @param data A categorical data set.
#' @param arities A vector of variable arities. 
#' @param yIndex The target variable index.
#' @param xIndices A vector of indices for the Xs. 
#' @export
mml_nb_adaptive = function(data, arities, yIndex, xIndices) {
  msgLen = 0
  # initialize variable count with 1 to avoid having 0 count
  # other values are also possible
  ycnt = rep(1, nlevels(data[, yIndex])) # count for each value of y
  # count for each value of x given each value of y, hence it is the cross 
  # product of arities of y and x
  if (length(xIndices) > 0) xcnt = rep(1, prod(arities[c(xIndices, yIndex)]))
  # update variable count on each data point
  for (rowID in 1:nrow(data)) {
    yValue = as.numeric(data[rowID, yIndex])
    p = ycnt / sum(ycnt) # marginal probability of y i.e., p(y)
    if (length(xIndices) > 0) {# if there is at least one x
      xValues = as.numeric(data[rowID, xIndices])
      q = rep(0, length(xcnt)) # joint distribution p(x, y)
      probX = 0
      # update p(x, y) for each value of y
      for (i in 1:arities[yIndex]) {
        q[((i - 1) * prod(arities[xIndices]) + 1):(prod(arities[xIndices]) * i)] = 
          (xcnt[((i - 1) * prod(arities[xIndices]) + 1):(prod(arities[xIndices]) * i)] / 
             sum(xcnt[((i - 1) * prod(arities[xIndices]) + 1):(prod(arities[xIndices]) * i)])) * p[i]
        
        index = node_value_to_index(arities[c(xIndices, yIndex)], c(xValues, i))
        # marginalize the p(x, y) for all y to get p(x)
        probX = probX + q[index] 
        # if y's ith value matches its current value then cach the current 
        # combination index
        if (i == yValue) cachedIndex = index
      }# end for each value of y
      msgLen = msgLen - log(q[cachedIndex] / probX) # upate message length
      xcnt[cachedIndex] = xcnt[cachedIndex] + 1 # update the corresponding count x|y by 1
    } else {# if there is no x
      msgLen = msgLen - log(p[yValue]) 
    }
    ycnt[yValue] = ycnt[yValue] + 1 # update count y by 1
  }# end of each data point
  return(msgLen)
}



