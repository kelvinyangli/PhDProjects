################################################################################
# evaluation on synthetic node orders
################################################################################
dags = list.files("dag/", pattern = ".rds")
ed_mtx = pre_mtx = rec_mtx = matrix(0, ncol = 12, nrow = 400*6)
alg = "sll"
col_cnt = 1
for (p in seq(0, 1, 0.1)) {
  row_cnt = 1
  for (nn in c(400,800,1600)) { # no morality
    for (i in 1:length(dags)) {
      name = strsplit(dags[i], ".rds")[[1]]
      files = list.files(paste0("mb_", alg, "_", nn), pattern = name)
      dag = readRDS(paste0("dag/", dags[i]))
      vars = bnlearn::nodes(dag)
      mr = dag2matrix(bnlearn::moral(dag))
      for (j in 1:length(files)) {
        ord2 = rev(readRDS(paste0("node_orders_random/", name, "_", p, ".rds")))
        if (alg == "sll") {
          mbcpt = sll2list(paste0("mb_", alg, "_", nn, "/", files[j]), vars)
        } else {
          mbcpt = readRDS(paste0("mb_", alg, "_", nn, "/", files[j]))
        }
        if (alg == "pcmb") {
          mbcpt_sym = symmetry_correction(vars, mbcpt, "intersection")
        } else if (alg == "sll") {
          mbcpt_sym = mbcpt
        } else {
          mbcpt_sym = symmetry_correction(vars, mbcpt, "union")
        }
        G = mblist2moral(mbcpt_sym, vars)
        # G1 = G
        # res = edit_dist_graph(G, mr)
        # ed_mtx[row_cnt, 12] = res$ed
        # pre_mtx[row_cnt, 12] = res$pre
        # rec_mtx[row_cnt, 12] = res$rec

        # G1 = wrsgraph::fixed_ordering_moralization(G, ord2)
        G1 = wrsgraph::moralization(G, ord2, 0)
        res = edit_dist_graph(G1, mr)
        ed_mtx[row_cnt, col_cnt] = res$ed
        pre_mtx[row_cnt, col_cnt] = res$pre
        rec_mtx[row_cnt, col_cnt] = res$rec

        row_cnt = row_cnt + 1
      }

    }

  }
  col_cnt = col_cnt + 1
}


write.xlsx(ed_mtx, paste0("random_node_order_triangulation_", alg, ".xlsx"), sheetName="ed", col.names=TRUE, row.names=F, append=T)
write.xlsx(pre_mtx, paste0("random_node_order_triangulation_", alg, ".xlsx"), sheetName="pre", col.names=TRUE, row.names=F, append=T)
write.xlsx(rec_mtx, paste0("random_node_order_triangulation_", alg, ".xlsx"), sheetName="rec", col.names=TRUE, row.names=F, append=T)

################################################################################
# evaluation on learned node orders
################################################################################
dags = list.files("dag/", pattern = ".rds")
ed_mtx = pre_mtx = rec_mtx = matrix(0, ncol = 7, nrow = 400*6)
for (col_cnt in 1:7) {
  
  row_cnt = 1
  for (nn in c(400,800,1600,3200,6400)) { # no morality
    for (i in 1:length(dags)) {
      dag = readRDS(paste0("dag/", dags[i]))
      root = node.ordering(dag)[1]
      vars = nodes(dag)
      mr = dag2matrix(moral(dag))
      name = strsplit(dags[i], ".rds")[[1]][1]
      files = list.files(paste0("mb_mmlcpt_", nn), pattern = name)
      
      for (j in 1:length(files)) {

        mbl = readRDS(paste0("mb_mmlcpt_", nn, "/", files[j]))
        mbl = symmetry_correction(vars, mbl, "union")
        g_mbl = mblist2moral(mbl, vars)

        if (col_cnt == 1) {
          g = min_deg_moralization(g_mbl)
        } else if (col_cnt == 2) {
          g = min_deficiency_moralization(g_mbl)
        } else if (col_cnt == 3) {
          ord = node.ordering(readRDS(paste0("mmhc_", nn, "/", files[j])))
          ord = rev(ord)
          g = moralization(g_mbl, ord, 1)
        } else if (col_cnt == 4) {
          tree = readRDS(paste0("chow_liu_", nn, "/", files[j]))
          ord = node.ordering(directing_tree(tree, root))
          ord = rev(ord)
          g = moralization(g_mbl, ord, 1)
        } else if (col_cnt == 5) {
          ord = node.ordering(readRDS(paste0("camml_", nn, "/", files[j])))
          ord = rev(ord)
          g = moralization(g_mbl, ord, 1)
        } else if (col_cnt == 6) {# sll only run for 400,800,1600 samples
          if (nn <= 1600) {
            dag_sll = read.table(paste0("sll_", nn, "/", strsplit(files[j], ".rds")[[1]][1]))
            colnames(dag_sll) = rownames(dag_sll) = vars
            ord = rev(node.ordering(matrix2dag(dag_sll)))
            g = moralization(g_mbl, ord, 1)  
          } else {
            g = mr
          }
          
        } else if (col_cnt == 7) {# pc
          dag_pc = readRDS(paste0("pc_", nn, "/", files[j]))
          ord = rev(node.ordering(dag_pc))
          g = moralization(g_mbl, ord, 1)
        }

        res = edit_dist_graph(g, mr)
        ed_mtx[row_cnt, col_cnt] = res$ed
        pre_mtx[row_cnt, col_cnt] = res$pre
        rec_mtx[row_cnt, col_cnt] = res$rec

        row_cnt = row_cnt + 1
      }

    }
  }

}

write.xlsx(ed_mtx, paste0("learned_node_order_moralization3.xlsx"), sheetName="ed", col.names=TRUE, row.names=F, append=T)
write.xlsx(pre_mtx, paste0("learned_node_order_moralization3.xlsx"), sheetName="pre", col.names=TRUE, row.names=F, append=T)
write.xlsx(rec_mtx, paste0("learned_node_order_moralization3.xlsx"), sheetName="rec", col.names=TRUE, row.names=F, append=T)


################################################################################
# measure kt distance of the learned orders
################################################################################
alg = "chow_liu"
dags = list.files("dag/", pattern = ".rds")
ktd = rep(0, 2400)
k = 1
for (nn in 100*c(4,8,16,32,64,128)) {
  for (i in 1:length(dags)) {
    dag = readRDS(paste0("dag/", dags[i]))
    ord_t = rev(node.ordering(dag))
    root = node.ordering(dag)[1]
    name = strsplit(dags[i], ".rds")[[1]][1]
    files = list.files(paste0(alg, "_", nn), name)
    for (j in 1:length(files)) {
      if (alg == "chow_liu") {
        ord_l = rev(node.ordering(directing_tree(readRDS(paste0(alg, "_", nn, "/", files[j])), root)))
      } else {
        ord_l = rev(node.ordering(readRDS(paste0(alg, "_", nn, "/", files[j]))))
      }

      ktd[k] = kendall_tau_distance(ord_t, ord_l)
      k = k + 1
    }
  }
}

m = matrix(ktd, ncol = 6, byrow = F)
1 - colMeans(m) / (49*25)

# camml
# 0.8419163 0.8982551 0.9231041 0.9423184 0.9522673 0.9610000
# mmhc
# 0.6677796 0.7032959 0.7508041 0.7918939 0.8303551 0.8623143
# chow liu mwst
# 0.5717592 0.5810408 0.5741980 0.5758367 0.5772878 0.5747816


# min degree and min deficiency
dags = list.files("dag/", pattern = ".rds")
ktd1 = ktd2 = rep(0, 2400)
k = 1
for (nn in c(400,800,1600,3200,6400,12800)) { # no morality
  for (i in 1:length(dags)) {
    dag = readRDS(paste0("dag/", dags[i]))
    vars = nodes(dag)
    ord_t = rev(node.ordering(dag))
    # mr = dag2matrix(moral(dag))
    name = strsplit(dags[i], ".rds")[[1]][1]
    files = list.files(paste0("mb_mmlcpt_", nn), pattern = name)

    for (j in 1:length(files)) {

      min_deg = min_dif = c()
      mbl = readRDS(paste0("mb_mmlcpt_", nn, "/", files[j]))
      mbl = symmetry_correction(vars, mbl, "union")
      g_mbl = mblist2moral(mbl, vars)

      graph = g_mbl
      H = graph
      while (length(graph) > 1) {
        # graph = prune_leaves(graph)
        nodes = colnames(graph)
        if (length(nodes) > 0) {
          # find the node with the smallest degree
          degrees = sapply(nodes, wrsgraph::degree, graph = graph)
          x = names(which.min(sample(degrees)))
          min_deg = c(min_deg, x)
          xNbrs = find_nbr(graph, x)
          for (y in xNbrs) {
            H[y, xNbrs[xNbrs != y]] = 1 # add deficiency
            graph[y, xNbrs[xNbrs != y]] = 0 # remove nbr edges
          }
          graph = wrsgraph::subgraph(graph, nodes = x, type = "nodes")
        }
      }

      min_deg = c(min_deg, vars[!vars %in% min_deg])
      ktd1[k] = kendall_tau_distance(ord_t, min_deg)

      graph = g_mbl
      H = graph
      while (length(graph) > 1) {
        # graph = prune_leaves(graph)
        nodes = colnames(graph)
        if (length(nodes) > 0) {
          # find the node with the smallest deficiency
          deficiencies = sapply(nodes, wrsgraph::deficiency, graph = graph)
          x = names(which.min(sample(deficiencies))) # randomize nodes
          min_dif = c(min_dif, x)
          xNbrs = find_nbr(graph, x)
          for (y in xNbrs) {
            H[y, xNbrs[xNbrs != y]] = 1 # add deficiency
            graph[y, xNbrs[xNbrs != y]] = 0 # remove nbr edges
          }
          graph = wrsgraph::subgraph(graph, nodes = x, type = "nodes")
        }
      }

      min_dif = c(min_dif, vars[!vars %in% min_dif])
      ktd2[k] = kendall_tau_distance(ord_t, min_dif)
      k = k + 1

    }
  }
}

m = matrix(ktd1, ncol = 6, byrow = F)
1 - colMeans(m) / (49*25)
# min degree
# 0.5384857 0.5368551 0.5415837 0.5456020 0.5541224 0.5575612

m = matrix(ktd2, ncol = 6, byrow = F)
1 - colMeans(m) / (49*25)
# min deficiency
# 0.5335041 0.5322327 0.5369163 0.5470735 0.5575735 0.5702000

