
# sample data from 1st order logit
beta0 <- 1
beta1 <- 0.5
xtest <- sample(c(0, 1), 10000, replace = T)
linpred <- beta0 + (xtest * beta1)
prob <- exp(linpred)/(1 + exp(linpred))
runis <- runif(10000,0,1)
ytest <- ifelse(runis < prob,1,0)

data1stOrder = data.frame(x = xtest, y = ytest)

glm(y~., family = binomial("logit"), data=data1stOrder)

# sample data from 2nd order logit
sampleSize = 50
beta0 <- 1
beta1 <- 10
beta2 = 20
beta3 = 15
x1 <- sample(c(0, 1), sampleSize, replace = T)
x2 = sample(c(0, 1), sampleSize, replace = T)
#linpred <- beta0 + beta1*x1 + beta2*x2
linpred <- beta0 + beta1*x1 + beta2*x2 + beta3*(x1*x2)
prob <- exp(linpred)/(1 + exp(linpred))
#runis <- runif(sampleSize,0,1)
#y <- ifelse(runis < prob,1,0)
y = rbinom(sampleSize, 1, prob)

data2ndOrder = data.frame(y, x1, x2)

#glm(y~x1+x2+x1*x2, family = binomial("logit"), data=data2ndOrder)

data = matrix("A", nrow = sampleSize, ncol = 3)
for (i in 1:nrow(data)) {
for (j in 1:ncol(data)) {
  if (data2ndOrder[i, j] == 1) data[i,j] = "B"
}
}
data=as.data.frame(data)
colnames(data) = c("y", "x1", "x2")
dd = dataInfo(data)
indicatorMatrix = getIndicator(data)

for (i in 1:10) {
  
  cpts = generateCPTs(dag, 2, 1)
  data = rbn(cpts, 5000)
  
  dd = dataInfo(data)
  indicatorMatrix = getIndicator(data)
  
  # mmlcpt
  mb.cpt = mbForwardSelection(data,"V2",mmlCPT,dd$arities,dd$indexListPerNodePerValue,2,indicatorMatrix=NULL,debug=F)
  cat("cpt   :", mb.cpt, "\n")
  
  # mmlLogit
  mb.logit1 = mbForwardSelection(data,"V2",mmlLogit,dd$arities,dd$indexListPerNodePerValue,2,indicatorMatrix,debug=F)
  cat("logit1:", mb.logit1, "\n")
  
  # mmlLogit2ndOrder
  mb.logit2 = mbForwardSelection(data,"V2",mmlLogit2ndOrder,dd$arities,dd$indexListPerNodePerValue,2,indicatorMatrix,debug=F)
  cat("logit2:", mb.logit2, "\n")
  
}

sampleSize = 1000
# xor data
x1 = sample(c(0, 1), sampleSize, replace = TRUE)
x2 = sample(c(0, 1), sampleSize, replace = TRUE)
y = x1 + x2 + x1 * x2
data = data.frame(x1, x2, y)
mbForwardSelection(data,"y",mmlLogit,c(2,2,2),NULL,2,indicatorMatrix,debug=T)
mbForwardSelection(data,"y",mmlLogit2ndOrder,c(2,2,2),NULL,2,indicatorMatrix,debug=T)




