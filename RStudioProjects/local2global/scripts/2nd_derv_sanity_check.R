

z = c(0.3,0.9)
g = function(z) {
  ss = 0
  for (i in 1:nrow(temp)) {
    yi = temp[i, 1]
    xij = temp[i, c(2,3)]
    pij1 = abs(abs(xij - 1) - z)
    ss = ss + yi * sum(log(pij1))
  }
  return(-ss)
}
g(z)
grad(g, z)
ss = 0
for (i in 1:nrow(temp)) {
  yi = temp[i, 1]
  xik = temp[i, 2]
  if (xik == 1) {
    pik1 = z[1]
  } else {
    pik1 = 1- z[1]
  }
  pik1 = z[1]
  ss = ss + yi / pik1
}
-ss

g = function(xx) {
  ss = 0 
  for (i in 1:nrow(temp)) {
    yi = temp[i, 1]
    xij = temp[i,c(2,3)]
    pij1 = abs(abs(xij-1)-xx[c(2,4)])
    #pij0 = abs(abs(temp[i,c(2,3)]-1)-xx[c(3,5)])
    ss = ss + yi*sum(log(pij1)) 
  } 
  return(-ss)
}
grad(g,xx)
ss = 0 
for (i in 1:nrow(data)) {
  yi = temp[i, 1]
  if (temp[i, 2] == 1) {
    pik1 = xx[4]
    ss = ss + (yi/pik1)
  } else {
    #pik1 = 1-xx[4]
    pik1 = xx[4]
    ss = ss + (-yi/pik1)
  }
}
-ss



ss = 0 
for (i in 1:nrow(data)) {
  yi = temp[i, 1]
  pij1 = c(pars$X1[data[i,2],1], pars$X2[data[i,3],1])
  pij0 = c(pars$X1[data[i,2],2], pars$X2[data[i,3],2])
  px = xx[1]*prod(pij1)+(1-xx[1])*prod(pij0)
  if (temp[i, 2] == 1) {
    pik1 = xx[2]
  } else {
    pik1 = 1-xx[2]
  }
  ss = ss + (yi/pik1 - xx[1]*prod(pij1)/(px*pik1))
}
-ss

############
#pars = mle_est_nb(data, vars, xIndices, yIndex) # mle of parameters with smoothing
# negative log likelihood
# this is partial nll if x is not empty
nll = -sum(apply(data, 1, nll_nb, pars = pars, xIndices = xIndices, yIndex = yIndex))

# p(y=T)
py1 = pars[[length(pars)]][[1]]
# p(y=F)
py0 = pars[[length(pars)]][[2]]
if (length(x) > 0) {# if x is not empty
  # a vector of \prod_j p(x_ij|y=1)
  prodPij1 = apply(data, 1, prod_pijk, pars = pars, xIndices = xIndices, yValue = 1)
  # a vector of \prod_j p(x_ij|y=2)
  prodPij0 = apply(data, 1, prod_pijk, pars = pars, xIndices = xIndices, yValue = 2)
  # a vector of p_xi 
  px = py1 * prodPij1 + py0 * prodPij0
  nll = nll + sum(log(px)) # add additional log(px) value to nll when x is not empty
  # a matrix of p(x_j|y=T) and p(x_j|y=F)
  probsMatrix = c()
  for (j in xIndices) {
    probsMatrix = 
      cbind(probsMatrix, apply(data, 1, p_ijk, pars = pars, xIndices = xIndices, xIndex = j, yValue = 1), apply(data, 1, p_ijk, pars = pars, xIndices = xIndices, xIndex = j, yValue = 2))
  }
  # FIM
  fim = fim_nb(prodPij1, prodPij0, px, probsMatrix, py1, py0, arities, xIndices, yIndex)
  # log determinant of FIM
  detFIM = det(fim)
  # for some reason, we have negative determinant when there is a small number of input variables,
  # since the problem can't be resolved for now, I manually convert negative determinant into 
  # positive. Most of negative determinant are very small negative values, meaning close to 0. 
  if (detFIM < 0) detFIM = -detFIM 
  logF = log(detFIM)
  # number of free parameters
  d = nrow(fim)
} else {# if x is empty fim is a 1x1 matrix 
  logF = log(sampleSize * (1 / py1 + 1 / py0))
  d = arities[yIndex] - 1
}
############



