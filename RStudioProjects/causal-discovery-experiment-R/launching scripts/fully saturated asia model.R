# test for bir between a learned model and a fully saturated model
cpts = read.dsc("C:/Experiments_23062016/asia/asia.dsc")

# create the fully saturated model of asia 
dagFull = model2network(modelstring(cpts))
dagFull = set.arc(dagFull, "asia", "smoke")
dagFull = set.arc(dagFull, "asia", "lung")
dagFull = set.arc(dagFull, "asia", "bronc")
dagFull = set.arc(dagFull, "asia", "either")
dagFull = set.arc(dagFull, "asia", "xray")
dagFull = set.arc(dagFull, "asia", "dysp")
dagFull = set.arc(dagFull, "tub", "smoke")
dagFull = set.arc(dagFull, "tub", "lung")
dagFull = set.arc(dagFull, "tub", "bronc")
dagFull = set.arc(dagFull, "tub", "either")
dagFull = set.arc(dagFull, "tub", "xray")
dagFull = set.arc(dagFull, "smoke", "either")
dagFull = set.arc(dagFull, "smoke", "xray")
dagFull = set.arc(dagFull, "smoke", "dysp")
dagFull = set.arc(dagFull, "lung", "bronc")
dagFull = set.arc(dagFull, "lung", "xray")
dagFull = set.arc(dagFull, "lung", "dysp")
dagFull = set.arc(dagFull, "bronc", "either")
dagFull = set.arc(dagFull, "bronc", "xray")
dagFull = set.arc(dagFull, "xray", "dysp")

data1 = rbn(cpts, 1000)
data2 = rbn(cpts, 10000)
data3 = rbn(cpts, 100000)
dataTestingNumeric = toNumeric(rbn(cpts, 10000)) 

cptsFull = bn.fit(dagFull, data1, method = "bayes")

dagLearned = hc(data1, score = "bde")
cptsLearned = bn.fit(dagLearned, data1, method = "bayes")

birs.full = prediction(cptsFull, dataTestingNumeric, 2, 2)
birs = prediction(cptsLearned, dataTestingNumeric, 2, 2)

birs.full$bir
birs$bir
mean(birs.full$bir)
mean(birs$bir)

### more data
cptsFull = bn.fit(dagFull, data2, method = "bayes")

dagLearned = hc(data2, score = "bde")
cptsLearned = bn.fit(dagLearned, data2, method = "bayes")

birs.full = prediction(cptsFull, dataTestingNumeric, 2, 2)
birs = prediction(cptsLearned, dataTestingNumeric, 2, 2)

birs.full$bir
birs$bir
mean(birs.full$bir)
mean(birs$bir)

### more data
cptsFull = bn.fit(dagFull, data3, method = "bayes")

dagLearned = hc(data3, score = "bde")
cptsLearned = bn.fit(dagLearned, data3, method = "bayes")

birs.full = prediction(cptsFull, dataTestingNumeric, 2, 2)
birs = prediction(cptsLearned, dataTestingNumeric, 2, 2)
bir.true = prediction(cpts, dataTestingNumeric, 2, 2)
birs.full$bir
birs$bir
bir.true$bir
mean(birs.full$bir)
mean(birs$bir)
mean(bir.true$bir)




