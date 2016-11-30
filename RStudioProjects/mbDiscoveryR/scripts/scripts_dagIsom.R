

ls = readRDS("all mb dags/mbDags6.rds")

ls = enumerateMBDags(paste0("V", 1:6), "T")
length(ls)
count = 0
dup = matrix(0, ncol = 2)
for (i in 1:(length(ls) - 1)) {
  if (!i %in% dup[,2]) {
  for (j in (i+1):length(ls)) {
      if (all.equal(ls[[i]], ls[[j]]) == TRUE) {
        count = count + 1
        dup = rbind(dup, c(i, j))
      }
    }
  }
}
(dup = dup[-1,])

v = rep(0, nrow(dup))
for (i in 1:nrow(dup)) {
  j = dup[i, 1]
  v[i] = length(parents(ls[[j]], "T")) + length(children(ls[[j]], "T"))
}

v = c()
for (i in 1:nrow(dup)) {
  j = dup[i, 1]
  if ((length(parents(ls[[j]], "T")) == 0) && length(children(ls[[j]], "T"))==3) v = c(v, j)
}
length(v)

w = c()
for (i in 1:length(v)) {
  j = v[i]
  if (max(sapply(children(ls[[j]], "T"), bnlearn::degree, object = ls[[j]])) == 2) {
    w = c(w, j)
  }
}
length(w)



