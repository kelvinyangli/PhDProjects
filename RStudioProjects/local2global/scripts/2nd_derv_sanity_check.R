sc2.f <- function(x){
  n <- length(x)
  sum((1:n) * (exp(x) - x)) / n
}
sc2.g <- function(x){
  n <- length(x)
  (1:n) * (exp(x) - 1) / n
}
x0 <- rnorm(5)
hess <- hessian(func=sc2.f, x=x0)
hessc <- hessian(func=sc2.f, x=x0, "complex")
all.equal(hess, hessc, tolerance = .Machine$double.eps)
# Hessian = Jacobian of the gradient
jac <- jacobian(func=sc2.g, x=x0)
jacc <- jacobian(func=sc2.g, x=x0, "complex")
all.equal(hess, jac, tolerance = .Machine$double.eps)
all.equal(hessc, jacc, tolerance = .Machine$double.eps)

x = c(pars$Y[[1]], pars$X1[2,1], pars$X1[2,2])
f = function(x) {
  p=x[1]
  q0=x[2]
  q1=x[3]
  px1 = q1*p+q0*(1-p)
  px2 = (1-q1)*p+(1-q0)*(1-p)
  ss=0
  for (i in 1:nrow(temp)) {
    if ((temp[i,1] == 1) && (temp[i,2]==1)) {
      ss=ss+log(q1*p/px1)
    } else if ((temp[i,2]==1) && (temp[i,1]==0)) {
      ss=ss+log(q0*(1-p)/px1)
    } else if ((temp[i,1]==1) && (temp[i,2]==0)) {
      ss=ss+log((1-q1)*p/px2)
    } else {
      ss=ss+log((1-q0)*(1-p)/px2)
    }
  }
  return(-ss)
}

h=hessian(func=f, x=x)

ss=0
p=x[1]
q0=x[2]
q1=x[3]
px1 = q1*p+q0*(1-p)
px2 = (1-q1)*p+(1-q0)*(1-p)
for (i in 1:nrow(temp)) {
  #ss = ss + temp[i,1]/p^2 + (1-temp[i,1])/(1-p)^2 - temp[i,2]*((q1-q0)/px1)^2 - (1-temp[i,2])*((1-q1-1+q0)/px2)^2
  ss = ss + temp[i,1]*temp[i,2]/q1^2 + (1-temp[i,2])*temp[i,1]/(1-q1)^2 - 
    temp[i,2]*(p^2)/(px1^2) - (1-temp[i,2])*(p^2)/px2^2
}
ss



