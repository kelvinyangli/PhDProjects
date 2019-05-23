
nvars = 50
maxNPas = 5
n = 300
# dag_true = readRDS("~/Documents/Experiments/kdd_exp/hailfinder/dag/hailfinder.rds")
dag_true = randDag(nvars, maxNPas)
cpts = randCPTs(dag_true, 2, 1)
data = rbn(cpts, n)
name = paste0(nvars, "_", maxNPas, "_2_1_", n)
write.csv(data, paste0(name, ".csv"), row.names = F)
dt = factor2numeric(data)
write.table(dt, paste0(name, ".dat"), row.names = F, col.names = F)

# learn mb using mmlcpt
vars = colnames(data)
data_cat = numeric2categorical(data)
arities = sapply(data_cat, nlevels)
di = varCnt = count_occurance(data_cat, arities)
data_num = data.matrix(data_cat)
registerDoParallel(3) # use 3 cores
mbcpt = foreach(target = vars,
    .combine = list,
    .multicombine = TRUE) %dopar% {
  forward_greedy_fast(data, di, arities, vars, n, target)
    }
stopImplicitCluster()
names(mbcpt) = vars
mbcpt_union = symmetry_correction(vars, mbcpt, "union")
mbcpt_inter = symmetry_correction(vars, mbcpt, "intersection")

# form an undirected graph
G = matrix(0, nvars, nvars)
dimnames(G) = list(vars, vars)
for (x in vars) {
  G[x, mbcpt_union[[x]]] = 1
}

# edit_dist_graph(dag2matrix(moral(sll)), mr)
mr = dag2matrix(bnlearn::moral(dag_true))
edit_dist_graph(G, mr)$ed
edit_dist_graph(dag2matrix(moral(sll)), mr)$ed
edit_dist_graph(dag2matrix(moral(dag_camml)), mr)$ed
edit_dist_graph(dag2matrix(moral(dag_camml_sk)), mr)$ed
edit_dist_graph(dag2matrix(moral(mmhc(data))), mr)$ed
edit_dist_graph(dag2matrix(moral(iamb(data))), mr)$ed

# make camml prior
mr = dag2matrix(bnlearn::skeleton(dag_true))
sk = dag2matrix(bnlearn::skeleton(dag_true))
vars = colnames(mr)
p = 0.8
#priors = rep(p, sum(mr)/2)
text = "arcs {"
for (i in 1:(nrow(mr)-1)) {
  for (j in (i+1):ncol(mr)) {
    if (mr[i,j]==1) text = paste(text, "\n", vars[i], "--", vars[j], p, ";")
  } # end for j
}
text = paste(text, "\n }")
write_file(text, "camml_true_moral_prior.txt")

text = "arcs {"
for (i in 1:(nrow(sk)-1)) {
  for (j in (i+1):ncol(sk)) {
    if (sk[i,j]==1) text = paste(text, "\n", vars[i], "--", vars[j], p, ";")
  } # end for j
}
text = paste(text, "\n }")
write_file(text, "camml_true_skeleton_prior.txt")


sll = read.table(paste0(name, "_sllg.dag"))
row.names(sll) = colnames(sll) = colnames(data)
sll = matrix2dag(sll)
dneFile = readr::read_file(paste0(name, ".dne"))
dag_camml = dne2bnlearn(dneFile)
dneFile = readr::read_file(paste0(name, "_skeleton_prior.dne"))
dag_camml_sk = dne2bnlearn(dneFile)
dneFile = readr::read_file(paste0(name, "_moral_prior.dne"))
dag_camml_moral = dne2bnlearn(dneFile)
#dag_by_immoralization = min_deg_moralization(mr)

# edit dist
bnlearn::shd(sll, dag_true)
bnlearn::shd(dag_camml, dag_true)
bnlearn::shd(dag_camml_sk, dag_true)
bnlearn::shd(dag_camml_moral, dag_true)

# kld, higher = better
aprx_kld(data, cpts, bn.fit(dag_true, data))
aprx_kld(data, cpts, bn.fit(sll, data))
aprx_kld(data, cpts, bn.fit(dag_camml, data))
aprx_kld(data, cpts, bn.fit(dag_camml_sk, data))
aprx_kld(data, cpts, bn.fit(dag_camml_moral, data))

#bnlearn::shd(matrix2dag(dag_by_immoralization), dag_true)
dag_edit_distance(dag2matrix(dag_camml), dag2matrix(dag_true))
dag_edit_distance(dag2matrix(dag_camml_moral), dag2matrix(dag_true))
dag_edit_distance(dag2matrix(dag_camml_sk), dag2matrix(dag_true))
#dag_edit_distance(dag_by_immoralization, dag2matrix(dag_true))

# test if the 1st mbcpt is direct nbr
# if x - y are highly likely to be direct nbr in dag
# then the edge shouldn't be removed during immoralization
y = rep(0, nvars)
lst = mbcpt_inter
for (i in 1:nvars) {
  if (lst[[i]][1] %in% dag_true$nodes[[i]]$nbr) y[i] = 1
}
sum(y) / nvars

m = matrix(0, nvars, nvars)
colnames(m) = rownames(m) = vars
for (i in 1:nvars) {
  #for (j in 1:nvars) {
    j = which(vars == mbcpt[[i]][1])
    m[i, j] = 1
  #}
}
node.ordering(dag_true)
