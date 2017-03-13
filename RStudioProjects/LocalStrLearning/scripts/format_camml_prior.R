# this script format the optimal local polytree structures learned into the
# required format for camml prior
vars = paste0("V", 1:40)
prob = 0.8
dir = "../../../Dag experiments/"
local_pts = list.files(paste0(dir, "local_pt"), "40_")

for (i in 1:length(local_pts)) {
  
  directed_arcs = c()
  local_pt = readRDS(paste0(dir, "local_pt/", local_pts[i]))
  
  for (j in 1:length(local_pt)) {
    
    directed_arcs = rbind(directed_arcs, get_arcs(local_pt[[j]]))
    
  } # end for j 
  
  directed_arcs = unique(directed_arcs)
  arc_list = find_bidirected_arcs(directed_arcs)
  text = format_camml_prior(arc_list)
  
  filename = strsplit(local_pts[i], ".rds")[[1]][1]
  write_file(text, paste0(dir, "prior/", filename, ".txt"))
  
}





# plot
#res_withPrior = netica2bnlearn(paste0("../../../Dag experiments/camml_withPrior_test/test.dne"))
#dag_camml_withPrior = parentsList2BN(res_withPrior)
#graphviz.plot(dag_camml_withPrior)

#dag = readRDS(paste0(dir, "dag/20_3_4_1_302793.rds"))
#editDistDags(dag_camml_withPrior, matrix2dag(dag$adjmtx))
#data = readRDS(paste0(dir, "data/20_3_4_1_302793_1000_431117.rds"))
#editDistDags(mmhc(data, score = "bde"), matrix2dag(dag$adjmtx))

