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
n = 200
#res_time = res_max_cs_size = res_is_moral = rep(0, n)
i = 1
while(i < (n+1)) {
  dag = randDag(300, 4)
  vars = nodes(dag)
  mbs = sapply(vars, mb, x=dag)
  delta = max(sapply(mbs, length))
  if (delta > 4) {
    dag = f(dag)
  }
  g = dag2matrix(moral(dag))
  cs = c(cs,max(components(graph_from_adjacency_matrix(g))$csize))
  time = system.time({isMoral = wrsgraph::d_wrs_deg4(g)})
  #res_is_moral[i] = isMoral
  tm = c(tm, time[3])
  if (i %% 50 == 0) cat(i, " ")
  i = i + 1
}

table(cs)
length(cs)
length(tm)

meanTm = c()
for (n in seq(5, 115, 5)) {
  meanTm = c(meanTm, mean(tm[which(cs == n)]))
}
plot(meanTm)

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

for (n in seq(5, 115, 5)) {
  for (i in 1:20) {
    cs = c(cs, n)
    g = rand_bounded_deg_graph(nvars, 4)
    time = system.time(d_wrs_deg4(g))
    tm = c(tm, time[3])
  }
  
}


results = results2 = rep(0, nreps)

for (i in 1:nreps) {
  results[i] = wrs_bktr(g_list[[i]], g_list[[i]])$wrs
  #results2[i] = d_wrs_deg4(g_list[[i]])
}

