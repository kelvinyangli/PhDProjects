# this functiom computes the mean, std and confidence interval of results
# results are saved in a matrix with 4 columns: precision, recall, distance, fmeasures
# the default confidence interval is 95%

computeCI = function(mtx, alpha = 0.05) {

  ss = dim(mtx)[1] # ss is sample size
  z = round(qnorm(alpha / 2, lower.tail = FALSE), 2) # quantile
  means = colMeans(mtx)
  stds = apply(mtx, 2, sd)
  
  #upper = means + z * stds/sqrt(ss)
  #lower = means - z * stds/sqrt(ss)
  
  x = z * stds/sqrt(ss)
  
  return(data.frame(means, x))
  
}