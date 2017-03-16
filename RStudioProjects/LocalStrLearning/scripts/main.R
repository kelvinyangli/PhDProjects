# this scripts conduct the following processes: 
# 1. generate random polytrees with given number of vars and max number of parents
# 2. generate random parameter values with give maximum arity and concentration parameter
# 3. learn mb for each var using mmlCPT with symmtry correction 
# 4. apply exhaustive polytree search within each mb according to mmlCPT
# 5. merge all learned local structures into a global structure, if an arc is bidirected 
#    then we make it undirected, if an arc is both unexisted and directed then we drop
#    the entire arc
# 6. compute edit distances for cpdags, skeletons, and dags for our mml method, 
#    mmhc, chow.liu and arance (notice the last two methods only return undirected polytrees)
# 7. use bootstrap to measure arc uncertainty (this step is optional, need to be further 
#    confirmed for its accuracy)

##############
# observations: 
# 1. random generated polytrees are all connected, but learned via mml could have 
#    isolated vars, fix this 
# 2. outstanding results for small samples, so we definitely test on small samples
#    against the others
##############

# read pre-saved mbpts into memory 
mbptsList = list()
for (i in 1:8) mbptsList[[i]] = readRDS(paste0("MBPTs/", i - 1, ".rds")) 

# log factorial sheet
logFactorialSheet = read.csv("logFactorial_1to10000.csv")

(dags_true = list.files(paste0(dir, "dag/")))
dag = readRDS(paste0(dir, "dag/", dags_true[1]))
dag = matrix2dag(dag$adjmtx)
graphviz.plot(dag)
(data_sets = list.files(paste0(dir, "data/")))
i = 3
data = readRDS(paste0(dir, "data/", data_sets[i]))
dataInfo = getDataInfo(data)
vars = colnames(data)
mbList = readRDS(paste0(dir, "mb/", data_sets[i]))
localStrs = readRDS(paste0(dir, "local_pt/", data_sets[i]))
(filename = strsplit(data_sets[i], ".rds")[[1]][1])
camml_withPrior = netica2bnlearn(paste0(dir, "camml_withPrior/", filename, ".dne"))
camml_withPrior = parentsList2BN(camml_withPrior)
editDistDags(camml_withPrior, dag)
camml_noPrior = netica2bnlearn(paste0(dir, "camml_noPrior/", filename, ".dne"))
camml_noPrior = parentsList2BN(camml_noPrior)
editDistDags(camml_noPrior, dag, TRUE)

# re-assess certainty 
featureUncertainty("V10", "V20", mbList, localStrs, mbptsList, vars, data, 5)

camml_test = netica2bnlearn(paste0(dir, "test.dne"))
camml_test = parentsList2BN(camml_test)
editDistDags(camml_test, dag)

count = 1
for (i in 1:20) {
  for (j in (1):20) {
    if (pt[i,j]>0) {
      cat(count, ":", vars[i], "->", vars[j], "#", pt[i,j], "\n")
      count = count + 1
    }
  }
}
# model specifications
nVars = 25
maxNPas = 4
maxArity = 5
beta = 1
n = 1000
maxMB = 7 
nExp = 10 # the number of times repeat this experiment
ed_dag = ed_pattern = ed_sklt = matrix(0, nExp, 3, dimnames = list(NULL, c("mml", "mmhc", "chow")))
for (ii in 1:nExp) { # repeat the process of learning nExp times for pt with the 
  # same specifications
  cat(ii, " ")
  # 1. generate random polytree structure
  adjmtx = randPolytree(nVars, maxNPas)
  pt = matrix2dag(adjmtx)
  #graphviz.plot(pt, main = "true")
  
  #real_model = read.dsc("../mbDiscoveryR/insurance.dsc")
  #graphviz.plot(real_model)
  #data = rbn(real_model, 1000)
  #vars = colnames(data)
  #pt = model2network(modelstring(alarm_network))
  
  # 2. generate random parameter values
  cpts = randCPTs(pt, maxArity, beta)
  data = rbn(cpts, n) 
  dataInfo = getDataInfo(data)
  vars = colnames(adjmtx)
  
  # 3. learn mb using mmlCPT
  #Rprof("mml.out")
  mbList = list()
  # learn mb(x), for all x \in vars
  for (i in 1:length(vars)) mbList[[i]] = mbForwardSelection.fast(data, vars[i], dataInfo$arities, dataInfo$indexListPerNodePerValue, base = exp(1))
  mbList = symmetryCorrection(vars, mbList) # apply symmetry correction 
  
  # restrict mb size to be <= 7 by dropping extra candidates
  for (i in 1:length(vars)) if (length(mbList[[i]]) > maxMB) mbList[[i]] = mbList[[i]][1:maxMB]
  
  # 4. learn local str for each var based on its learned mb 
  # 5. merging local structures into global structure
  mbpt_global = learnMBPT(vars, mbList, mbptsList, dataInfo, n)
  #Rprof(NULL)
  #proftable("mml.out")
  pt_mml = matrix2dag(mbpt_global)
  
  # related works
  mmhc_bnlearn = mmhc(data, score = "bde") # mmhc w/ bde
  chow_bnlearn = chow.liu(data) # chow and liu with mutual information
  
  # 6. evaluate learners using edit distance for dag, pattern and skeleton
  ed_dag[ii, ] = c(editDistDags(pt_mml, pt), editDistDags(mmhc_bnlearn, pt), editDistDags(chow_bnlearn, pt))
  ed_pattern[ii, ] = c(bnlearn::shd(pt_mml, pt), bnlearn::shd(mmhc_bnlearn, pt), bnlearn::shd(chow_bnlearn, pt))
  ed_sklt[ii, ] = c(hamming(pt_mml, pt), hamming(mmhc_bnlearn, pt), hamming(chow_bnlearn, pt))
  
} # end for ii

df_dag = rbind(
colMeans(ed_dag) - apply(ed_dag, 2, sd),
colMeans(ed_dag),
colMeans(ed_dag) + apply(ed_dag, 2, sd))
df_pattern = rbind(
colMeans(ed_pattern) - apply(ed_pattern, 2, sd),
colMeans(ed_pattern),
colMeans(ed_pattern) + apply(ed_pattern, 2, sd))
df_sklt = rbind(
colMeans(ed_sklt) - apply(ed_sklt, 2, sd),
colMeans(ed_sklt),
colMeans(ed_sklt) + apply(ed_sklt, 2, sd))
dimnames(df_dag) = dimnames(df_pattern) = dimnames(df_sklt) = list(NULL, c("mml", "mmhc", "chow"))

round(df_dag, 2)
round(df_pattern, 2)
round(df_sklt, 2)

# greedy search for better global str
#mmlDag(mbpt_global, vars, dataInfo, n)
#mmlDag(adjmtx, vars, dataInfo, n)

# ploting
#graphviz.plot(matrix2dag(mbpt_global), main = "mml")
#graphviz.plot(mmhc(data), main = "mmhc")
#graphviz.plot(chow.liu(data), main = "chow.liu")
#graphviz.plot(aracne(data), main = "aracne")















