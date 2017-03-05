# dirichlet-multinomial distribution
# it is a family of discrete multivariate probability distributions 
# also known as dirichlet compound multinomial distribution (DCM)
# the pmf is (n!*gamma(alpha_0)/gamma(n+alpha_0))*prod_1^k(gamma(x_k+alpha_k)/(x_k!*gamma(alpha_k)))
# where n is the number of trials, alpha_k is the parametetr for dirichlet at kth coordinate, when it is symmetric dirichlet alpha_k = 1
# alpha_0 = sum_1^k(alpha_k), alpha_0 = k for symmetric dirichlet, gamma(x) = (x - 1)!
# see wikipedia page for details

# the following function computes the pmf for dirichlet-multinomial distribution
# it takes three inputs, n = number of trials, x = (x_1, x_2, ..., x_k), alpha = (alpha_1, alpha_2, ..., alpha_k)
# both x and alpha are vectors of length k, where k corresponds to the number of states for a multinomial distribution
# this function should get the same answer as the uniform prior used for mml multistate distribution
# i.e 1/choose(n + m - 1, m - 1), where n = number of trials, m = number of states 

# this function has potential over flow problem, because both gamma and factorial do not work for large integers

dirichletMultinomial = function(n, x, alpha) {
  
  # check if some conditions are satisfied
  if (length(x) != length(alpha)) {
    
    print("x and alpha must have the same length!")
    
  } else if (sum(x) != n) {
    
    print("Totoal number of observations in x does not equal to n!")
    
  } else {# if both conditions above are satisfied then run the following code to compute the probability 
    
    alpha_0 = sum(alpha) # alpha_0 = sum_1^k(alpha_k)
    
    product = 1
    
    for (k in 1:length(alpha)) {
      
      product = product * gamma(x[k] + alpha[k]) / (factorial(x[k]) * gamma(alpha[k]))
      
    } # end for k
    
    product = product * factorial(n) * gamma(alpha_0) / gamma(n + alpha_0) 
    
    return(product)
    
  } # end else 
  
}


