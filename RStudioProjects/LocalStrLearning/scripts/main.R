# In this script, we use full search for MBPTs and compare against others
#
# Test 1: compare with polytree search algorithms. The objective is to demonstarte that
# by searching through all possible MBPTs, we are able to find the MBPT that is
# a sub-graph of the true local structure within the MB. Since the returned str is a
# polytree, we may want to compare our method with polytree learning algorithm without
# considering true and false negatives.

adjmtx = randAdjmtx(7, 2)
dag = matrix2dag(adjmtx)
graphviz.plot(dag)
cpts = randCPTs(dag, 2, 1)
data = rbn(cpts, 1000)

# extract mb(x) from the true model
x = "V2"
y = mBlkt(adjmtx, x)
n = length(y) # size of mb
mbpts = readRDS(paste0("MBPTs/", n, ".rds")) # load pre-saved mbpts for mb size n

# 



# a function to extract the bn of a var and its local str


par(mfrow = c(1, 2))
graphviz.plot(dag)
graphviz.plot(matrix2dag(mbLocalStr(adjmtx, "V5")))

a = 0:7
b = c(1, 1, 2, 3, 5, 7, 10, 13) # n files for each value n \in [0, 7]
for (i in 3:8) {
  files = list.files("MBPTs/", paste0(a[i], "_"))[1:b[i]]
  ls = list()
  for (j in 1:length(files)) ls = c(ls, readRDS(paste0("MBPTs/", files[j])))
  saveRDS(ls, paste0(a[i], ".rds"))
}




