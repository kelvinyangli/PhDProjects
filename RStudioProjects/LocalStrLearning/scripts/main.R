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

dir = "../../../Dag experiments/"
(dags_true = list.files(paste0(dir, "dag/")))
nData = 8
dag = readRDS(paste0(dir, "dag/", dags_true[nData]))
dag = matrix2dag(dag$adjmtx)
graphviz.plot(dag)
(data_sets = list.files(paste0(dir, "data/"), strsplit(dags_true[nData], ".rds")[[1]][1]))
i = 4
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
mtx = mergeMBPTs(localStrs, vars)
arcs = extractArcs(mtx)
priors = arcPrior(arcs, maxProb = 0.8, mbList, localStrs, mbptsList, vars, data, nResamples = 5 )
text = cammlPrior(arcs, priors)
write_file(text, paste0(dir, "test_prior.txt"))



camml_test = netica2bnlearn(paste0(dir, "test.dne"))
camml_test = parentsList2BN(camml_test)
editDistDags(camml_test, dag, debug = T)

camml_test = netica2bnlearn(paste0(dir, "test_prior.dne"))
camml_test = parentsList2BN(camml_test)
editDistDags(camml_test, dag, debug = T)

count = 1
for (i in 1:20) {
  for (j in (1):20) {
    if (pt[i,j]>0) {
      cat(count, ":", vars[i], "->", vars[j], "#", pt[i,j], "\n")
      count = count + 1
    }
  }
}





















