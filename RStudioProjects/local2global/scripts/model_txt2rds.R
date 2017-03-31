
  dag = read.table("../../../UAI_exp/insurance3/dag/Insurance3_graph.txt")
  row.names(dag) = colnames(dag)
  dag = matrix2dag(dag)
  saveRDS(dag, "../../../UAI_exp/insurance3/dag/insurance3.rds")






