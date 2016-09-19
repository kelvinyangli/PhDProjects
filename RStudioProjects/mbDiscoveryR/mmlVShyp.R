# this is a simple test b/w mml and hyp test on two simple models
# the aim is to compare the accuracies of mml and hyp test on a really simple case
# this was mentioned by Lloyd on friday's meeting 26/08/2016
dag1 = empty.graph(c("V1", "V2")) # V1 V2
dag2 = dag1
dag2= set.arc(dag2, "V1", "V2") # V1 -> V2

n = 10
nTrials = 1000
alpha = 0.01

actual = rep(0, nTrials)
mml = hyp = actual

for (i in 1:nTrials) {
  
  if (runif(1) < 0.5) {
    
    dag = dag1
    cpts = generateCPTs(dag, 2, 1)
    data = rbn(cpts, n)
    actual[i] = 1
    
  } else {
    
    dag = dag2
    cpts = generateCPTs(dag, 2, 1)
    data = rbn(cpts, n)
    actual[i] = 2
    
  }
  
  # compute msg len 
  dataInfo = getDataInfo(data)
  
  # compute msg len for dag1 being the correct model
  # total msglen = msglen(V2|V1) + msglen(V1)
  msglen2 = mmlCPT(1, c(), dataInfo$indexListPerNodePerValue, dataInfo$arities, n, base = exp(1)) + 
    mmlCPT(2, 1, dataInfo$indexListPerNodePerValue, dataInfo$arities, n, base = exp(1))
  
  # compute msg len for dag2 being the correct model
  msglen1 = mmlCPT(1, c(), dataInfo$indexListPerNodePerValue, dataInfo$arities, n, base = exp(1)) + 
    mmlCPT(2, c(), dataInfo$indexListPerNodePerValue, dataInfo$arities, n, base = exp(1))
  
  # predict based on msglen
  if (msglen1 < msglen2) {
    
    mml[i] = 1
    
  } else {
    
    mml[i] = 2
    
  } 
  
  # dependence test, null: ind; alt: dep
  pvalue = ci.test("V1", "V2", data = data)$p.value
  
  # predict based on hypthethis test
  if (pvalue < alpha) {
    
    hyp[i] = 2
    
  } else {
    
    hyp[i] = 1
    
  }
  
}

sum(actual == mml)/nTrials
sum(actual == hyp)/nTrials

# results: 
# n = 10, 1000 trials: mml 62%; hyp (0.01) 54%
# n = 100, 1000 trials: mml 77%; hyp (0.01) 73%
# n = 1000, 1000 trials: mml 89%; hyp (0.01) 89%
# n = 10000, 100 trials: mml 99%; hyp (0.01) 97%; hyp (0.05) 95%
# n = 10000, 1000 trials: mml 96%; hyp (0.01) 96%; hyp (0.05) 94%; 

