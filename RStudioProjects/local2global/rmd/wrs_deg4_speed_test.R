# test excution time on max deg 4 graphs
# below results are for all moral graphs, generated from random bns
# 10 - 0.0025
# 20 (-2.25) - 0.0043
# 40 (-3.9) - 0.0099
# 80 (-9) - 0.0193
# 160 (-19.8) - 0.06095
#
# 120 (-13) - 0.0332
# 1100 (-132) - 5.154
n = 1000
res_time = res_max_cs_size = res_is_moral = rep(0, n)
i = 1
while(i < n) {
  dag = randDag(100, 4)
  vars = nodes(dag)
  mbs = sapply(vars, mb, x=dag)
  delta = max(sapply(mbs, length))
  if (delta > 4) {
    dag = f(dag)
  }
  g = dag2matrix(moral(dag))
  res_max_cs_size[i] = max(components(graph_from_adjacency_matrix(g))$csize)
  tm = system.time({isMoral = wrsgraph::d_wrs_deg4(g)})
  res_is_moral[i] = isMoral
  res_time[i] = tm[3]
  cat(i, " ")
  i = i + 1
}
mean(res_time)
unique(res_is_moral)
res_max_cs_size

zz = c()
for (i in seq(10,70,10)) zz = c(zz, mean(res_time[which(res_max_cs_size == i)]))

graphviz.plot(matrix2dag(g))
graphviz.plot(dag)

mbs = sapply(vars, mb, x = dag)
sapply(mbs, length)


dag = randDag(20, 6)
graphviz.plot(dag)
graphviz.plot(f(dag))

mb2moral = function(mbs) {
  vars = names(mbs)
  m = matrix(0, length(vars), length(vars))
  colnames(m) = rownames(m) = vars
  for (i in 1:length(vars)) {
    mb = mbs[[i]]
    m[vars[i], mb] = 1
  }
  return(m)
}


f = function(dag) {
  vars = bnlearn::nodes(dag)
  repeat {
    mbs = sapply(vars, mb, x = dag)
    maxMBS = max(sapply(mbs, length))
    if (maxMBS > 4) {
      delta = sapply(mbs, length)
      bigNodes = names(which(delta > 4))
      x = sample(bigNodes, 1)
      y = sample(dag$nodes[[x]]$nbr, 1)
      dag = drop.arc(dag, x, y)
    } else {
      break
    }
  }
  return(dag)
}

mean(res)

nvars = 20
nreps = 100
g_list = list()
for (i in 1:nreps) {
  g_list[[i]] = rand_bounded_deg_graph(nvars, 3)
}


results = results2 = rep(0, nreps)
ptm = proc.time()
for (i in 1:nreps) {
  results[i] = wrs_bktr(g_list[[i]], g_list[[i]])$wrs
  results2[i] = d_wrs_deg4(g_list[[i]])
}
proc.time() - ptm
sum(results - results2)
sum(results2)

