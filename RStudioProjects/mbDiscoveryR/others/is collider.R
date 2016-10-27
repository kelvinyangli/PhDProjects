# test if three nodes form a collider
is.collider = function(x, y, z, data) {
  # the structure is x-z-y
  
  # marginal mi
  marginalMI = ci.test(x, y, data = data)$statistic
  
  # conditional mi
  condMI = ci.test(x, y, z, data = data)$statistic
  
  return(c(marginalMI, condMI))
}