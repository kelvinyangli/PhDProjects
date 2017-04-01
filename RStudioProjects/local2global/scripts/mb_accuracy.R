model = "barley"
dir = paste0("../../../UAI_exp/", model, "/")
n = 5000
nRepeat = 10
dag = readRDS(paste0(dir, "dag/", model, ".rds"))
vars = bnlearn::nodes(dag)
#mtx = matrix(0, nRepeat * length(vars), 4)
(mbs = list.files(paste0(dir, "mb/", n)))
#k = 1
#v = c()
w = c()
for (i in 1:length(mbs)) {
  mbList = readRDS(paste0(dir, "mb/", n, "/", mbs[i]))
  v = c()
  for (j in 1:length(mbList)) {
    res = mbEditDist(bnlearn::mb(dag, vars[j]), mbList[[j]], vars[j], vars)
    v = c(v, res)
    #mtx[k, 1] = res[[1]]
    #mtx[k, 2] = res[[2]]
    #mtx[k, 3] = sqrt((1 - res[[1]])^2 + (1 - res[[2]])^2)
    #if ((mtx[k,1]==0) && (mtx[k,2]==0)) {
    #  mtx[k,4]=0
    #} else {
    #  mtx[k, 4] = 2 * res[[1]] * res[[2]] / (res[[1]] + res[[2]])  
    #}
    
    #k = k + 1
  }
  w = c(w, sum(v))
} # end for i
cat(model, "-", n, "\n")
cat(round(mean(w),2), "\n")
cat(round(1.96*sd(w)/sqrt(length(w)),2))


